// Draw all tiles at depth 10 (put in an object's Draw event)
// Assumes:
//   global.mapWidth, global.mapHeight
//   global.tilemap is a 1D array sized (mapWidth + 1) * (mapHeight + 1) or mapWidth * mapHeight
// Choose one convention and keep it consistent.
// This code uses width/height as counts (32 means tiles 0..31).

depth = 10;

var tileSize;
tileSize = global.tileSize

var mx;
var my;

for (my = 0; my < global.mapHeight; my += 1)
{
    for (mx = 0; mx < global.mapWidth; mx += 1)
    {
        var idx;
        idx = mx + my * global.mapWidth;

        var t;
        t = global.tilemap[idx];

        if (t == 0)
        {
            continue;
        }

        var spr;
        spr = noone;

        if (t == global.tileTNut)
        {
            spr = spr_nut;
        }
        else if (t == global.tileTBuriedNut)
        {
            spr = spr_buried_nut;
        }
        else if (t == global.tileTBranch)
        {
            spr = spr_branch;
        }

        if (spr != noone)
        {
            draw_sprite(spr, 0, mx * tileSize, my * tileSize);
        }
    }
}

