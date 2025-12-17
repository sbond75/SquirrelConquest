var _enemy;
_enemy = argument0

var _moveSpeed;
_moveSpeed = argument1

var _tileSize;
_tileSize = global.tileSize

// Target: random tile in tile space
var target_tileX;
var target_tileY;
target_tileX = _enemy.tileX + irandom_range(-1, 1);
target_tileY = _enemy.tileY + irandom_range(-1, 1);

// Target: random tile in pixel space
var target_x;
var target_y;
target_x = target_tileX * _tileSize;
target_y = target_tileY * _tileSize;

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

    //_enemy.x += nx * _moveSpeed * _tileSize;
    //_enemy.y += ny * _moveSpeed * _tileSize;
    _enemy.tileX += nx * _moveSpeed;
    _enemy.tileY += ny * _moveSpeed;
}

// Check wrap
if _enemy.tileX < 0 {
    _enemy.tileX = 0
}
if _enemy.tileX >= global.mapWidth {
    _enemy.tileX = global.mapWidth - 1
}
if _enemy.tileY < 0 {
    _enemy.tileY = 0
}
if _enemy.tileY >= global.mapHeight {
    _enemy.tileY = global.mapHeight - 1
}
// Apply movement to sprite/obj
_enemy.x = _enemy.tileX * _tileSize
_enemy.y = _enemy.tileY * _tileSize
