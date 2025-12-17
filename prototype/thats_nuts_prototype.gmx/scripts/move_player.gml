var moveSpeed
moveSpeed = argument0

var dx;
var dy;

dx = 0;
dy = 0;

// Collect input
if (keyboard_check(ord("A")))
{
    dx -= moveSpeed;
}
if (keyboard_check(ord("D")))
{
    dx += moveSpeed;
}
if (keyboard_check(ord("W")))
{
    dy -= moveSpeed;
}
if (keyboard_check(ord("S")))
{
    dy += moveSpeed;
}

// Only move if there is some input
if (dx != 0 || dy != 0)
{
    var newX;
    var newY;

    newX = tileX + dx;
    newY = tileY + dy;
    
    // Check wrap
    if newX < 0 {
        newX = 0
    }
    if newX >= global.mapWidth {
        newX = global.mapWidth - 1
    }
    if newY < 0 {
        newY = 0
    }
    if newY >= global.mapHeight {
        newY = global.mapHeight - 1
    }

    // When not on a branch, move freely.
    // When on a branch, only allow moving onto another branch tile.
    if (!onBranch || get_tile(newX, newY) == global.tileTBranch)
    {
        tileX = newX;
        tileY = newY;
    }
}

// Apply movement to sprite/obj
x = tileX * global.tileSize
y = tileY * global.tileSize
