#version 440 core

layout(location=0) out vec4 frag_color;

layout(location=4) in vec4 color;

void main()
{
	frag_color = color;
}