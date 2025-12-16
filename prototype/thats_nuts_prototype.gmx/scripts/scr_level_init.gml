// Map size in tiles
global.mapWidth = 32
global.mapHeight = 32

// 32x32 1D array. 0 = air, 1 = branch
global.tileTAir = 0
global.tileTBranch = 1
global.tilemapSize = global.mapWidth * global.mapHeight
var i;
for (i = 0; i < global.tilemapSize; i += 1)
{
    global.tilemap[i] = 0
}

// sprite/tile size in pixels
global.tileSize = 8

// Enemies
global.numEnemies = 16
global.enemyCounter = 0
for (i = 0; i < global.numEnemies; i += 1)
{
    global.enemies[i] = noone
}

// Nuts
global.numNuts = 16
global.nutCounter = 0
for (i = 0; i < global.numNuts; i += 1)
{
    global.nuts[i] = noone
}
