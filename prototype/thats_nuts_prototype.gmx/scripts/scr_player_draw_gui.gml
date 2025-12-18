// debug
draw_text(0,16,string(tileX) + ", " + string(tileY))

// hp
var pixelScale;
pixelScale = 10 * global.tileSize
draw_rectangle_colour(0,0,hp * pixelScale,pixelScale,c_black,c_black,c_black,c_black,false)

// score
draw_text(64, 0, score)
