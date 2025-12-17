var tileX;
tileX = argument0
var tileY;
tileY = argument1
var buried
buried = argument2
var thrown
thrown = argument3
var dx
dx = argument4
var dy
dy = argument5
var high
high = argument6

var tileSize;
tileSize = global.tileSize; // tile size in pixels

var px;
var py;
px = tileX * tileSize;
py = tileY * tileSize;
var obj;
obj = instance_create(px, py, obj_nut)
obj.buried = buried
if buried {
    obj.sprite_index = spr_buried_nut
}
if thrown {
    obj.thrown = true
    obj.dx = dx
    obj.dy = dy
}
else {
    obj.thrown = false
    obj.dx = 0
    obj.dy = 0
}
obj.high = high

// Find slot in global.nuts
var i;
var found;
found = false
for (i = 0; i < global.numNuts; i++)
{
    if global.nuts[i] == noone {
        global.nuts[i] = obj
        found = true
        break
    }
}
if !found {
    show_error("no room in nuts", true)
}
