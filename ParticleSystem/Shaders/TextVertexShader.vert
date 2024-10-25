#version 440 core

layout(location=0) in vec4 vert;
layout(location=1) out vec2 texture_coords;

layout(location=28) uniform mat4 u_ViewProjection;

void main()
{
	gl_Position = u_ViewProjection*vec4(vert.xy,0,1);
	texture_coords = vert.zw;
}