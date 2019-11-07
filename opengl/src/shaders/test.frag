uniform float u_time;
uniform vec2 resolution;

void main(void)
{
    vec2 position = gl_FragCoord.xy / resolution;
    gl_FragColor = vec4(position.x, 0.0, position.y, 1.0);
}