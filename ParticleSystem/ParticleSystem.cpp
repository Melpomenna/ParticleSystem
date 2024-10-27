#include <condition_variable>
#include <queue>
#include <random>
#include <vector>
#include <array>

#include <GL/glew.h>

#include <GLFW/glfw3.h>

#include <fstream>
#include <functional>
#include <iostream>
#include <string>
#include <unordered_map>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#include <freetype/freetype.h>

#define POINTS_PER_CLICK 512
#define DEFAULT_POINT_SPEED 75

struct RGBAColor final
{
	RGBAColor(float r, float g, float b, float a) noexcept : r(r), g(g), b(b), a(a)
	{
	}

	float r, g, b, a;
};

struct Position2D final
{
	Position2D(float x, float y) noexcept : x(x), y(y)
	{
	}

	float x, y;
};


struct Vector2D final
{
	Vector2D(float x, float y) : x(x), y(y)
	{
	}

	float x, y;
};

struct Point2D final
{
	Point2D(Position2D pos, RGBAColor color, size_t idx, Vector2D speed) noexcept : color(color), pos(pos), idx(idx),
		speed(speed), dt(0), deadFlag(false)
	{
	}

	RGBAColor color;
	Position2D pos;
	size_t idx;
	Vector2D speed;
	double dt;
	bool deadFlag;
};

class Timer
{
public:
	Timer() : last_(std::chrono::steady_clock::now())
	{
	}


	double Mark() noexcept
	{
		auto prev = last_.load();
		last_.store(std::chrono::steady_clock::now());

		return std::chrono::duration<double>(last_.load() - prev).count();
	}

	double Peek() const noexcept
	{
		return std::chrono::duration<double>(std::chrono::steady_clock::now() - last_.load()).count();
	}

private:
	std::atomic<std::chrono::steady_clock::time_point> last_;
};

struct Character
{
	GLuint TextureID;
	glm::ivec2 Size;
	glm::ivec2 Bearing;
	GLuint Advanced;
};

void MouseCallback(GLFWwindow* window, int button, int action, int modes);
void CreatePointsThread() noexcept;
void PhysicsThread(float, float, int) noexcept;
void BindPoints(GLuint, GLuint) noexcept;
char* ReadShader(const char*) noexcept;
void DrawText(const std::string&, GLuint, GLuint, GLuint, const std::unordered_map<unsigned char, Character>&,
              glm::vec4, Position2D);

class PointVector
{
public:
	PointVector() : points_(nullptr), size_(0), capacity_(0)
	{
	}

	~PointVector()
	{
		allocator_.deallocate(points_, capacity_.load());
	}

	template <class... Args>
	void EmplaceBack(Args&&... args)
	{
		if (!points_)
		{
			points_ = allocator_.allocate(capacity_.load());
		}

		if (size_ == capacity_)
		{
			capacity_.store(capacity_.load() * 2);
			Point2D* ptr = allocator_.allocate(capacity_.load());
			memcpy(ptr, points_, size_.load() * sizeof(Point2D));
			allocator_.deallocate(points_, capacity_.load());
			points_ = ptr;
		}
		Point2D point{std::forward<Args>(args)...};
		allocator_.construct(points_ + size_.load(), point);
		size_.fetch_add(1);
	}

	void Erase(const size_t from, const size_t to, const size_t deadCount)
	{
		if (static_cast<int>(size_.load()) - static_cast<int>(deadCount) < 0)
		{
			size_.store(0);
			return;
		}
		qsort(points_, to - from + 1, sizeof(Point2D), [](const void* lhs, const void*)
		{
			if (*static_cast<const bool*>(lhs))
			{
				return 1;
			}
			return -1;
		});
		size_.fetch_sub(deadCount);
	}

	Point2D& operator[](const size_t idx) noexcept
	{
		return points_[idx];
	}

	const Point2D& operator[](const size_t idx) const noexcept
	{
		return points_[idx];
	}

	void PushBack(const Point2D& point)
	{
		EmplaceBack(point);
	}

	void Reserve(const size_t size)
	{
		points_ = allocator_.allocate(size);
		capacity_ = size;
	}

	size_t Size() const noexcept
	{
		return size_.load();
	}

	Point2D* Data() noexcept
	{
		return points_;
	}

	const Point2D* Data() const noexcept
	{
		return points_;
	}

private:
	std::allocator<Point2D> allocator_;
	Point2D* points_;
	std::atomic<size_t> size_;
	std::atomic<size_t> capacity_;
};

template <class T>
class UnboundingMPMCQueue
{
public:
	using ValueType = T;

	UnboundingMPMCQueue() = default;
	virtual ~UnboundingMPMCQueue() = default;

	template <class Q>
	void Push(Q&& value)
	{
		std::unique_lock<std::mutex> lock{mt_};
		values_.emplace(std::forward<Q>(value));
		notEmpty_.notify_one();
	}

	T TopAndPop()
	{
		std::unique_lock<std::mutex> lock{mt_};
		while (values_.empty())
		{
			notEmpty_.wait(lock);
		}
		auto value = values_.front();
		values_.pop();
		return value;
	}

private:
	std::queue<ValueType> values_;
	mutable std::mutex mt_;
	mutable std::condition_variable notEmpty_;
};

template <size_t Size>
class StaticThreadPool final
{
public:
	using HandleType = std::function<void()>;

	StaticThreadPool()
	{
		for (size_t i = 0; i < Size; ++i)
		{
			pool_[i] = std::thread(&StaticThreadPool::Execute, this);
		}
	}

	~StaticThreadPool()
	{
		Join();
	}

	template <class T, class... Args>
	void Push(T&& handle, Args&&... args)
	{
		if (!handle)
		{
			return;
		}

		auto binded = std::bind(std::forward<T>(handle), std::forward<Args>(args)...);

		auto handler = [h = binded]() noexcept
		{
			try
			{
				h();
			}
			catch (...)
			{
			}
		};

		handlers_.Push(std::move(handler));
	}

	void Join()
	{
		for (size_t i = 0; i < Size; ++i)
		{
			handlers_.Push(nullptr);
		}
		for (size_t i = 0; i < Size; ++i)
		{
			if (pool_[i].joinable())
			{
				pool_[i].join();
			}
		}
	}

	constexpr size_t Count() const noexcept
	{
		return Size;
	}

private:
	void Execute()
	{
		while (true)
		{
			auto handler = handlers_.TopAndPop();
			if (handler == nullptr)
			{
				return;
			}
			handler();
		}
	}

private:
	std::array<std::thread, Size> pool_;
	UnboundingMPMCQueue<HandleType> handlers_;
};

namespace
{
	PointVector POINTS = {};
	std::mt19937_64 GENERATOR;

	std::condition_variable NOT_CREATED_POINTS = {};
	std::queue<Position2D> MOUSE_POSITIONS = {};
	std::mutex CREATE_MOUSE_POINTS_MT = {};
	std::mutex CREATE_POINTS_MT = {};
	std::unique_ptr<Timer> TIMER;

	std::atomic<bool> IS_RUNNING = false;
	std::atomic<size_t> POINTS_COUNT = 0;
	StaticThreadPool<20> POOL;
}

int main()
{
	IS_RUNNING = false;
	POINTS.Reserve(POINTS_PER_CLICK * POINTS_PER_CLICK);

	POOL.Push(CreatePointsThread);

	{
		std::random_device device;
		GENERATOR = std::mt19937_64(device());
	}

	glfwInit();

	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 4);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

	const auto width = 800, height = 640;


	for (auto i = 0; i < POOL.Count() - 1; ++i)
	{
		POOL.Push(PhysicsThread, width, height, i);
	}

	const char* title = "ParticleSystem";

	GLFWwindow* window = glfwCreateWindow(width, height, title, nullptr, nullptr);


	glfwMakeContextCurrent(window);

	glewInit();

	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_PROGRAM_POINT_SIZE);

	glViewport(0, 0, width, height);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();

	glOrtho(-width, width, -height, height, -1.0, 1.0);

	auto model = glm::mat4(1);
	auto projection = glm::ortho<float>(-width, width, -height, height, -1.0, 1.0);
	auto viewProjection = model * projection;

	glfwSetMouseButtonCallback(window, MouseCallback);


	glfwShowWindow(window);


	//Text

	FT_Library ft;
	FT_Init_FreeType(&ft);

	FT_Face font;
	FT_New_Face(ft, "Font/Silver.ttf", 0, &font);

	std::unordered_map<unsigned char, Character> characters;

	FT_Set_Pixel_Sizes(font, 0, 67);

	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

	for (unsigned char i = 0; i < 255; ++i)
	{
		if (FT_Load_Char(font, i, FT_LOAD_RENDER))
		{
			continue;
		}

		GLuint texture;
		glGenTextures(1, &texture);
		glBindTexture(GL_TEXTURE_2D, texture);

		glTexImage2D(
			GL_TEXTURE_2D,
			0,
			GL_RED,
			font->glyph->bitmap.width,
			font->glyph->bitmap.rows,
			0,
			GL_RED,
			GL_UNSIGNED_BYTE,
			font->glyph->bitmap.buffer
		);

		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
		glGenerateMipmap(GL_TEXTURE_2D);

		Character sym = {
			texture,
			glm::ivec2(font->glyph->bitmap.width, font->glyph->bitmap.rows),
			glm::ivec2(font->glyph->bitmap_left, font->glyph->bitmap_top),
			font->glyph->advance.x
		};

		characters.emplace(i, sym);
	}

	FT_Done_Face(font);
	FT_Done_FreeType(ft);

	auto textShaderProgram = glCreateProgram();

	{
		const GLuint vertexShader = glCreateShader(GL_VERTEX_SHADER);
		const GLuint fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);

		auto vertexSource = ReadShader("Shaders/TextVertexShader.vert");
		auto fragmentSource = ReadShader("Shaders/TextFragmentShader.frag");

		glShaderSource(vertexShader, 1, &vertexSource, nullptr);
		glShaderSource(fragmentShader, 1, &fragmentSource, nullptr);

		glCompileShader(vertexShader);
		glCompileShader(fragmentShader);

		glAttachShader(textShaderProgram, vertexShader);
		glAttachShader(textShaderProgram, fragmentShader);
		glLinkProgram(textShaderProgram);
		glUseProgram(textShaderProgram);

		glValidateProgram(textShaderProgram);

		glDetachShader(textShaderProgram, vertexShader);
		glDetachShader(textShaderProgram, fragmentShader);

		glDeleteShader(vertexShader);
		glDeleteShader(fragmentShader);

		delete vertexSource;
		delete fragmentSource;
	}

	{
		const auto location = glGetUniformLocation(textShaderProgram, "u_ViewProjection");
		glUniformMatrix4fv(location, 1, GL_FALSE, glm::value_ptr(viewProjection));
	}

	{
		const auto location = glGetUniformLocation(textShaderProgram, "u_textureSampler");
		glUniform1i(location, 0);
	}

	GLuint textVBO, textVAO;
	glGenVertexArrays(1, &textVAO);
	glGenBuffers(1, &textVBO);
	glBindVertexArray(textVAO);
	glBindBuffer(GL_ARRAY_BUFFER, textVAO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(float) * 6 * 4, NULL, GL_DYNAMIC_DRAW);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 4 * sizeof(float), 0);
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindVertexArray(0);


	//

	GLuint vbo, vao;

	auto shaderProgram = glCreateProgram();

	{
		const GLuint vertexShader = glCreateShader(GL_VERTEX_SHADER);
		const GLuint fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);

		auto vertexSource = ReadShader("Shaders/VertexShader.vert");
		auto fragmentSource = ReadShader("Shaders/FragmentShader.frag");

		glShaderSource(vertexShader, 1, &vertexSource, nullptr);
		glShaderSource(fragmentShader, 1, &fragmentSource, nullptr);

		glCompileShader(vertexShader);
		glCompileShader(fragmentShader);

		glAttachShader(shaderProgram, vertexShader);
		glAttachShader(shaderProgram, fragmentShader);
		glLinkProgram(shaderProgram);
		glUseProgram(shaderProgram);

		glValidateProgram(shaderProgram);

		glDetachShader(shaderProgram, vertexShader);
		glDetachShader(shaderProgram, fragmentShader);

		glDeleteShader(vertexShader);
		glDeleteShader(fragmentShader);

		delete vertexSource;
		delete fragmentSource;
	}


	glGenVertexArrays(1, &vao);
	glGenBuffers(1, &vbo);


	auto location = glGetUniformLocation(shaderProgram, "u_ViewProjection");
	glUniformMatrix4fv(location, 1, GL_FALSE, glm::value_ptr(viewProjection));


	BindPoints(vbo, vao);


	TIMER = std::make_unique<Timer>();

	IS_RUNNING = true;

	std::string text = "PointsCount:";

	while (!glfwWindowShouldClose(window))
	{
		TIMER->Mark();
		glClear(GL_COLOR_BUFFER_BIT);
		glClearColor(0, 0, 0, 1);

		glUseProgram(shaderProgram);

		BindPoints(vbo, vao);
		glDrawArrays(GL_POINTS, 0, POINTS.Size());

		text = "Points Count:" + std::to_string(POINTS.Size());
		DrawText(text, textShaderProgram, textVBO, textVAO, characters, {1, 1, 1, 1}, {-width, height - 100});

		glfwSwapBuffers(window);
		glfwPollEvents();
	}
	IS_RUNNING = false;
	NOT_CREATED_POINTS.notify_all();

	glDeleteProgram(shaderProgram);
	glDeleteProgram(textShaderProgram);

	for (const auto& item : characters)
	{
		glDeleteTextures(1, &item.second.TextureID);
	}

	glfwDestroyWindow(window);
	glfwTerminate();

	POOL.Join();

	return 0;
}

char* ReadShader(const char* file) noexcept
{
	std::ifstream stream{file};

	stream.seekg(0, std::ios::end);

	const auto size = stream.tellg();

	stream.seekg(0, std::ios::beg);

	char* result = ::new char[size]();

	stream.read(result, size);

	return result;
}

void BindPoints(const GLuint vbo, const GLuint vao) noexcept
{
	glBindVertexArray(vao);


	glBindBuffer(GL_ARRAY_BUFFER, vbo);

	glBufferData(GL_ARRAY_BUFFER, static_cast<GLsizeiptr>(POINTS.Size() * sizeof(Point2D)), POINTS.Data(),
	             GL_DYNAMIC_DRAW);


	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(Point2D), (void*)offsetof(Point2D, pos));

	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(Point2D), (void*)offsetof(Point2D, color));
}

void CreatePointsThread() noexcept
{
	while (!IS_RUNNING)
	{
		std::this_thread::yield();
	}

	while (IS_RUNNING)
	{
		Position2D pos = {0, 0};
		{
			std::unique_lock<std::mutex> lock{CREATE_MOUSE_POINTS_MT};

			while (MOUSE_POSITIONS.empty())
			{
				NOT_CREATED_POINTS.wait(lock);
				if (!IS_RUNNING)
				{
					return;
				}
			}

			pos = MOUSE_POSITIONS.front();
			MOUSE_POSITIONS.pop();
		}


		std::unique_lock<std::mutex> lock{CREATE_POINTS_MT};
		for (auto i = 0; i < POINTS_PER_CLICK; ++i)
		{
			POINTS.EmplaceBack(pos, RGBAColor(static_cast<float>(GENERATOR() % 255) / 255.0f,
			                                  static_cast<float>(GENERATOR() % 255) / 255.0f,
			                                  static_cast<float>(GENERATOR() % 255) / 255.0f,
			                                  static_cast<float>(GENERATOR() % 255) / 255.0f),
			                   POINTS_COUNT.load(), Vector2D{
				                   DEFAULT_POINT_SPEED / 2.0f + GENERATOR() % DEFAULT_POINT_SPEED,
				                   DEFAULT_POINT_SPEED / 2.0f + GENERATOR() % DEFAULT_POINT_SPEED
			                   });
			POINTS_COUNT.fetch_add(1);
		}
		if (POINTS_COUNT.load() >= POINTS_PER_CLICK * POINTS_PER_CLICK)
		{
			POINTS_COUNT.store(0);
		}
	}
}


void PhysicsThread(const float w, const float h, const int id) noexcept
{
	constexpr float angleDelta = 360.0f / POINTS_PER_CLICK;

	float sinValues[POINTS_PER_CLICK];
	float cosValues[POINTS_PER_CLICK];
	for (auto i = 0; i < POINTS_PER_CLICK; ++i)
	{
		const float angle = angleDelta * static_cast<float>(i);
		sinValues[i] = std::sinf(angle);
		cosValues[i] = std::cosf(angle);
	}

	while (!IS_RUNNING)
	{
		std::this_thread::yield();
	}

	constexpr auto workersCount = POOL.Count() - 1;

	while (IS_RUNNING)
	{
		const auto shift = POINTS.Size() / workersCount;
		const size_t from = id * shift;
		const size_t to = shift + from;

		const float dt = static_cast<float>(TIMER->Peek());

		size_t deadPoints = 0;

		for (size_t i = from; i < to && i < POINTS.Size(); ++i)
		{
			auto& point = POINTS[i];
			if (point.deadFlag)
			{
				deadPoints++;
				continue;
			}
			const auto idx = i % POINTS_PER_CLICK;

			const float time = static_cast<float>(point.dt * point.dt);
			point.pos.y += sinValues[idx] * point.speed.y * dt - 4.9f * time * dt;
			point.pos.x += cosValues[idx] * point.speed.x * dt;

			point.dt += dt;

			if (point.pos.x > w || point.pos.y > h || point.pos.x < -w || point.pos.y < -h)
			{
				point.deadFlag = true;
				++deadPoints;
			}
		}

		if (deadPoints <= (to - from) / 2)
		{
			continue;
		}
		std::unique_lock<std::mutex> lock{CREATE_POINTS_MT};
		POINTS.Erase(from, to, deadPoints);
	}
}

void MouseCallback(GLFWwindow* window, const int button, const int action, const int)
{
	if (button == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_PRESS)
	{
		double x, y;
		glfwGetCursorPos(window, &x, &y);
		int w, h;
		glfwGetWindowSize(window, &w, &h);

		std::unique_lock<std::mutex> lock{CREATE_MOUSE_POINTS_MT};
		MOUSE_POSITIONS.emplace(2.0 * (x - w / 2.0), 2.0 * (h / 2.0 - y));
		NOT_CREATED_POINTS.notify_one();
	}
}


void DrawText(const std::string& text, GLuint shader, GLuint vbo, GLuint vao,
              const std::unordered_map<unsigned char, Character>& characters, glm::vec4 color, Position2D pos)
{
	glUseProgram(shader);
	glUniform4f(glGetUniformLocation(shader, "u_color"), color.x, color.y, color.z, color.w);
	glActiveTexture(GL_TEXTURE0);
	glBindVertexArray(vao);

	float x = pos.x;
	float y = pos.y;

	for (auto c = text.begin(); c != text.end(); ++c)
	{
		const Character ch = characters.at(*c);

		const float xpos = x + ch.Bearing.x;
		const float ypos = y - (ch.Size.y - ch.Bearing.y);

		const float w = ch.Size.x;
		const float h = ch.Size.y;

		const float vertices[6][4] = {
			{xpos, ypos + h, 0.0f, 0.0f},
			{xpos, ypos, 0.0f, 1.0f},
			{xpos + w, ypos, 1.0f, 1.0f},

			{xpos, ypos + h, 0.0f, 0.0f},
			{xpos + w, ypos, 1.0f, 1.0f},
			{xpos + w, ypos + h, 1.0f, 0.0f}
		};

		glBindTexture(GL_TEXTURE_2D, ch.TextureID);

		glBindBuffer(GL_ARRAY_BUFFER, vbo);
		glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(vertices), vertices);
		glBindBuffer(GL_ARRAY_BUFFER, 0);

		glDrawArrays(GL_TRIANGLES, 0, 6);

		x += (ch.Advanced >> 6);
	}

	glBindVertexArray(0);
	glBindTexture(GL_TEXTURE_2D, 0);
}
