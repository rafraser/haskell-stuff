uniform vec2 resolution;

vec4 colorize(int i) {
    if(i == 0 || i == 50) {
        return vec4(0.0, 0.0, 0.0, 1.0);
    } else {
        float strength = float(i)/50.0;
        return vec4(0.5*strength, 1.5*strength, 2.0*strength, 1.0);
    }
}

void main(void)
{
    vec2 position = gl_FragCoord.xy / resolution;
    vec2 z, c;
    c.x = -0.5 + (position.x - 0.5) * 3.0;
    c.y = (position.y - 0.5) * 3.0;
    
    z = c;
    int i;
    for(i=0; i<50; i++) {
        float real = (z.x * z.x - z.y * z.y) + c.x;
        float imag = (z.y * z.x + z.x * z.y) + c.y;
        
        if((real*real + imag*imag) > 4.0) break;
        
        z.x = real;
        z.y = imag;
    }
    
    gl_FragColor = colorize(i);
}