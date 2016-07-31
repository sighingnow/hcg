#version 430

varying vec4 position_v;

in vec4 color;
in vec2 tex_coord;
uniform sampler2D sampler;

void main() {
    gl_FragColor = texture2D(sampler, tex_coord.st);
}
