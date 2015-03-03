#version 150

uniform vec4 drawColor;
in vec2 norm;
out vec4 outColor;

void main()
{
  //~ outColor = vec4(0.0, 0.0, 0.0, 1.0);
	//~ outColor = vec4(norm.xy, drawColor.z, 1.0);
	outColor = drawColor;
}
