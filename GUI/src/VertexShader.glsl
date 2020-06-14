#version 330 core
layout (location = 0) in vec3 aPos; // the position variable has attribute position 0
layout (location = 1) in vec2 aTexCoord; // the texture variable has attribute position 2

out vec2 TexCoord;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main()
{
    // see how we directly give a vec3 to vec4's constructor
    gl_Position = vec4(aPos, 1.0) * model * view * projection;
    TexCoord = vec2(aTexCoord.x, 1.0f-aTexCoord.y);
}
