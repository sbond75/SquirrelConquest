var tileX;
tileX = argument0
var tileY;
tileY = argument1
var buried
buried = argument2

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
