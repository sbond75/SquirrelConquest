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

    // When not on a branch, move freely.
    // When on a branch, only allow moving onto another branch tile.
    if (!onBranch || get_tile(newX, newY) == global.tileTBranch)
    {
        tileX = newX;
        tileY = newY;
    }
}

