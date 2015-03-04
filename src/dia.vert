#version 150

uniform mat4 proj;
in vec2 position;
in vec2 normal;

out vec2 norm;

void main()
{
	norm = normal;
  gl_Position = proj * vec4(position, 0.0, 1.0);
  //~ gl_Position = vec4(position * 0.3, 0.0, 1.0);
}
