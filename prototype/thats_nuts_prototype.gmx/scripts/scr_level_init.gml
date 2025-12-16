// Map size in tiles
global.mapWidth = 32
global.mapHeight = 32

// 32x32 1D array. 0 = air, 1 = nut, 2 = buried nut, 3 = branch
global.tileTAir = 0
global.tileTNut = 1
global.tileTBuriedNut = 2
global.tileTBranch = 3
global.tilemapSize = global.mapWidth * global.mapHeight
var i;
for (i = 0; i < global.tilemapSize; i += 1)
{
    global.tilemap[i] = 0
}

// sprite/tile size in pixels
global.tileSize = 8
