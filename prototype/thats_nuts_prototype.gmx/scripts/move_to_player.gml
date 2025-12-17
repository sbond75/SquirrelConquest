var _enemy;
_enemy = argument0

var _moveSpeed;
_moveSpeed = argument1

var _tileSize;
_tileSize = global.tileSize

// Target: player's tile in pixel space
var target_x;
var target_y;
target_x = obj_squirrel.tileX * _tileSize;
target_y = obj_squirrel.tileY * _tileSize;

// Target: player's tile in tile space
var target_tileX;
var target_tileY;
target_tileX = obj_squirrel.tileX;
target_tileY = obj_squirrel.tileY;

// Vector from enemy to target
var dx;
var dy;
dx = target_x - _enemy.x;
dy = target_y - _enemy.y;

// Clamp movement so we do not overshoot
//if (abs(dx) <= 1 && abs(dy) <= 1)
if (dx * dx + dy * dy <= 1)
{
    // Snap exactly to target
    _enemy.x = target_x;
    _enemy.y = target_y;
    _enemy.tileX = target_tileX;
    _enemy.tileY = target_tileY;
}
else
{
    // Move toward target by moveSpeed * _tileSize pixels
    var nx;
    var ny;
    nx = dx;
    ny = dy;
    if nx > 1 {
        nx = 1;
    }
    if nx < -1 {
        nx = -1;
    }
    if ny > 1 {
        ny = 1;
    }
    if ny < -1 {
        ny = -1;
    }

    _enemy.x += nx * _moveSpeed * _tileSize;
    _enemy.y += ny * _moveSpeed * _tileSize;
    _enemy.tileX += nx * _moveSpeed;
    _enemy.tileY += ny * _moveSpeed;
}
