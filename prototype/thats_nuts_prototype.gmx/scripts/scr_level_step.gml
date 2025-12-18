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
     
            if enemy.canDamagePlayer and enemy.damagedPlayerFrameCounter > 0 and (abs(dx) <= 1 && abs(dy) <= 1)
            {
                move_to_player(enemy, 1);
            }
            
            // Damage if on top of player
            if (abs(dx) <= 0 && abs(dy) <= 0)
            {
                if obj_squirrel.onBranch {
                    // Can't damage from here
                    enemy.canDamagePlayer = false
                    enemy.damagedPlayerFrameCounter = 0
                }
                else {
                    damage_player(enemy);
                }
            }
            else {
                enemy.damagedPlayerFrameCounter++
            }
            
            if enemy.damagedPlayerFrameCounter > 1 and (abs(dx) >= 1 && abs(dy) >= 1) {
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
            
            if enemy.canDamagePlayer and get_tile(tileX, tileY) == global.tileTAir and (global.frameCounter mod 5 == 0) {
                // move idly
                move_idle(enemy, 1)
            }
            
            if enemy.canDamagePlayer and enemy.damagedPlayerFrameCounter > 10 and enemy.moveCounter < 5
            {
                move_to_player(enemy, 1);
                // Don't move too fast:
                enemy.moveCounter += 5
            }
            
            // Damage if on top of player
            if (abs(dx) <= 0 && abs(dy) <= 0)
            {
                damage_player(enemy);
            }
            else {
                enemy.damagedPlayerFrameCounter++
            }
            
            if enemy.damagedPlayerFrameCounter > 3 and (abs(dx) > 2 && abs(dy) > 2)
            {
                enemy.canDamagePlayer = true
            }
            
            enemy.moveCounter--
        }
    }
}

// Update acorns
var i;
var nut;
nut = noone
var tileX;
var tileY;
for (i = 0; i < global.nutCounter; i += 1)
{
    if (global.nuts[i] != noone)
    {        
        nut = global.nuts[i]
        
        if nut.thrown {
            // Look for an enemy instance exactly on this tile
            var en;
            en = noone
            if !nut.high {
                en = collision_rectangle(
                    nut.bbox_left,
                    nut.bbox_top,
                    nut.bbox_right,
                    nut.bbox_bottom,
                    obj_rattlesnake,
                    false,  // precise = false (bbox is fine and cheaper)
                    false   // notme = false (player is not obj_nut anyway)
                );
            }
            if en == noone and nut.high {
                en = collision_rectangle(
                    nut.bbox_left,
                    nut.bbox_top,
                    nut.bbox_right,
                    nut.bbox_bottom,
                    obj_eagle,
                    false,  // precise = false (bbox is fine and cheaper)
                    false   // notme = false (player is not obj_nut anyway)
                );
            }
            
            if en != noone {
                tileX = nut.tileX
                tileY = nut.tileY
                //instance_destroy(nut);
                //global.nuts[i] = noone;
                acorn_destroy(tileX, tileY, true)
                
                enemy_destroy(tileX, tileY)
                continue
            }
        }
        
        nut.tileX = nut.tileX + nut.dx
        nut.tileY = nut.tileY + nut.dy
        nut.x = nut.x + nut.dx * global.tileSize;
        nut.y = nut.y + nut.dy * global.tileSize;
    }
}

global.frameCounter++
