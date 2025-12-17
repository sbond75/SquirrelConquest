// Update enemies
var i;
var enemy;
enemy = noone
var tileX;
var tileY;
for (i = 0; i < global.enemyCounter; i += 1)
{
    if (global.enemies[i] != noone)
    {
        enemy = global.enemies[i]
        tileX = enemy.tileX
        tileY = enemy.tileY
        if (enemy.object_index == obj_rattlesnake) {
            // Check for player a tile away or less and insta-attack the player
            // Difference in tile coordinates
            var dx;
            var dy;
            
            dx = obj_squirrel.tileX - tileX;
            dy = obj_squirrel.tileY - tileY;
            
            // Euclidean distance in tiles: distance <= 1
            // (same tile or any of the 4 cardinal neighbors)
            //if (dx * dx + dy * dy <= 1)
            // If instead want diagonals to count as “within 1 tile” as well (so 8 neighbors + same tile), use this version:
            // Chebyshev distance: max(|dx|, |dy|) <= 1
            //if (abs(dx) <= 1 && abs(dy) <= 1)
     
            if enemy.damagedPlayerFrameCounter > 0 and (abs(dx) <= 1 && abs(dy) <= 1)
            {
                move_to_player(enemy, 1);
            }
            
            if (abs(dx) <= 0 && abs(dy) <= 0)
            {
                damage_player(enemy);
            }
            else {
                enemy.damagedPlayerFrameCounter++
            }
            
            if enemy.damagedPlayerFrameCounter > 1 {
                enemy.canDamagePlayer = true
            }
        }
        else if (enemy.object_index == obj_eagle) {
            // Check for player 2 tiles away or less and walk to the player
            // Difference in tile coordinates
            var dx;
            var dy;
            
            dx = obj_squirrel.tileX - tileX;
            dy = obj_squirrel.tileY - tileY;
            
            if enemy.damagedPlayerFrameCounter > 3 and (abs(dx) <= 2 && abs(dy) <= 2)
            {
                move_to_player(enemy, 1);
            }
            
            // Damage if on top of player
            if (abs(dx) <= 0 && abs(dy) <= 0)
            {
                damage_player(enemy);
            }
            else {
                enemy.damagedPlayerFrameCounter++
            }
            
            if enemy.damagedPlayerFrameCounter > 3 and (abs(dx) > 1 && abs(dy) > 1)
            {
                enemy.canDamagePlayer = true
            }
        }
    }
}
