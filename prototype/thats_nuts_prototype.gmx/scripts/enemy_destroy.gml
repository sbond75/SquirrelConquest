var tileX
tileX = argument0
var tileY
tileY = argument1

var tileSize;
tileSize = global.tileSize

// Remove from global.nuts by setting the entry to noone
var i;
var enemy;
enemy = noone
for (i = 0; i < global.enemyCounter; i += 1)
{
    if (global.enemies[i] != noone && global.enemies[i].tileX == tileX && global.enemies[i].tileY == tileY)
    {
        enemy = global.enemies[i]
        global.enemies[i] = noone;
        break;
    }
}

if enemy == noone {
    show_error("enemy not found at " + string(tileX) + ", " + string(tileY), true)
}

// Destroy the enemy instance
with (enemy)
{
    instance_destroy();
}

// // Check victory
// var i;
// var enemy;
// enemy = noone
// for (i = 0; i < global.enemyCounter; i += 1)
// {
//     if (global.enemies[i] != noone)
//     {
//         enemy = global.enemies[i]
//         break;
//     }
// }
// if enemy == noone {
//     show_message("Victory!")
// }
