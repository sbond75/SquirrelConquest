var tileX
tileX = argument0
var tileY
tileY = argument1
var makeNew
makeNew = argument2

var tileSize;
tileSize = global.tileSize

 // Remove from global.nuts by setting the entry to noone
var i;
var nut;
nut = noone
for (i = 0; i < global.nutCounter; i += 1)
{
    if (global.nuts[i] != noone && global.nuts[i].tileX == tileX && global.nuts[i].tileY == tileY)
    {
        nut = global.nuts[i]
        global.nuts[i] = noone;
        break;
    }
}

if nut == noone {
    show_error("nut not found at " + string(tileX) + ", " + string(tileY), true)
}

// Destroy the acorn instance
with (nut)
{
    instance_destroy();
}

if makeNew {
    // Make a new one at a random place far away
    random_pos_away_from_player(10)
    acorn_create(out_rndPosX, out_rndPosY, false, false, 0, 0, false)
}
