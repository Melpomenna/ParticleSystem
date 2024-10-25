#version 440 core

layout(location=0) out vec4 frag_color;
layout(location=1) in vec2 texture_coords;


layout(location=16) uniform sampler2D u_textureSampler;
layout(location=17) uniform vec4 u_color;

void main()
{
	vec4 sampled = vec4(1,1,1,texture(u_textureSampler, texture_coords).r);
	frag_color = u_color*sampled;
}