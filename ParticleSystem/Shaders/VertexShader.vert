#version 440 core

layout(location=0) in vec4 pos;
layout(location=1) in vec4 in_color;

layout(location=4) out vec4 color;
layout(location=28) uniform mat4 u_ViewProjection;

void main()
{
	gl_PointSize = 10;
	gl_Position = u_ViewProjection*vec4(pos.xy, 0, 1);
	color = in_color;
}