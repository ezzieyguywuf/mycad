#version 330 core

// The position of the vertex
layout (location = 0) in vec3 aPos;
// Which way is the line going?
layout (location = 1) in vec3 dir;
// The color of the vertex
layout (location = 2) in vec4 aColor;
// Which way to draw the thickness
layout (location = 3) in float up;

out vec4 VertexColor;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
uniform float aspect;
uniform float thickness;

void main() {
    // First, transform points to the so-called "clip space"
    vec4 projectedPos = vec4(aPos, 1.0) * model * view * projection;
    vec4 projectedDir = vec4(dir, 1.0)  * model * view * projection;

    // Next, do aspect ratio stuff to convert to "screen space"
    vec2 aspectVec = vec2(aspect, 1.0);
    vec2 screenPos = projectedPos.xy / projectedPos.w * aspectVec;
    vec2 screenDir = projectedDir.xy / projectedDir.w * aspectVec;

    // This vector is the direction of the line in screen-space
    vec2 dir = normalize(screenDir - screenPos);

    // since it's 2-d, we can easily calculate the normal
    vec2 normal = vec2(-dir.y, dir.x);

    // The normal is the direction we'll 'extrude' the line, by half the thickness
    normal *= thickness/2.0;
    // Not sure about these maths...
    normal.x /= aspect;



    // This delta is where our original point needs to go
    vec4 delta = vec4(0.0f);
    if(up > 0)
    {
        delta = vec4(normal, 0.0, 1.0);
    }
    else
    {
        delta = vec4(-1 * normal, 0.0, 1.0);
    }

    // Finally, transform our point.
    gl_Position = projectedPos + delta;
    //gl_PointSize = 1.0;
}
