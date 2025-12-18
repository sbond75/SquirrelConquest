// hp
var pixelScale;
pixelScale = 10 * global.tileSize
draw_rectangle_colour(0,0,hp * pixelScale,pixelScale / 2,c_black,c_black,c_black,c_black,false)

// score
draw_text(view_wport / 2, 0, score)

// debug
draw_set_colour(c_white)
draw_text(16,16,string(tileX) + ", " + string(tileY))
draw_text(16,32,global.enemyCounter)
