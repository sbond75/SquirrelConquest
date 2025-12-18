var excludeRadius;
excludeRadius = argument0; // in tiles
var excludeRadiusSq;
excludeRadiusSq = excludeRadius * excludeRadius;

var centerX;
var centerY;
centerX = global.mapWidth / 2;
centerY = global.mapHeight / 2;

var tileSize;
tileSize = global.tileSize; // tile size in pixels

while true {
    var tileX;
    tileX = irandom(global.mapWidth-1);
    
    var tileY;
    tileY = irandom(global.mapHeight-1);
    
    // Skip if center is inside the excluded center kernel
    var cdx;
    var cdy;
    cdx = tileX - centerX;
    cdy = tileY - centerY;

    if (cdx * cdx + cdy * cdy <= excludeRadiusSq)
    {
        continue;
    }
    
    is_occupied(tileX, tileY, false)
    if out_isOccupied {
        continue
    }
 
    out_rndPosX = tileX
    out_rndPosY = tileY   
    break;
}
