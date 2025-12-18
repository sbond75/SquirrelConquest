var lim
lim = argument0

var tileSize
tileSize = global.tileSize

var kernelRadius;
kernelRadius = 2; // radius in tiles

var centerX;
var centerY;
centerX = argument1;
centerY = argument2;

var excludeRadius;
excludeRadius = 5;
var excludeRadiusSq;
excludeRadiusSq = excludeRadius * excludeRadius;



randomEnemyChance = 0.015;  // 1.5% per non-branch tile



var mx
var my
while global.enemyCounter < lim {
    for (my = 0; my < global.mapHeight; my += 1)
    {
        for (mx = 0; mx < global.mapWidth; mx += 1)
        {
            var tileType;
            tileType = get_tile(mx, my);
    
            var chance;
            chance = 0;
            
            is_occupied(mx, my, true)
            if out_isOccupied {
                continue
            }
    
            if (tileType == global.tileTAir)
            {
                chance = randomEnemyChance;
            }
            
            // Check limit
            if global.enemyCounter >= lim {
                break;
            }
            
            // Skip if center is inside the excluded center kernel
            var cdx;
            var cdy;
            cdx = mx - centerX;
            cdy = my - centerY;
            if (cdx * cdx + cdy * cdy <= excludeRadiusSq)
            {
                continue;
            }
    
            if (chance > 0 && random(1) < chance)
            {
                // Create enemy aligned to tile
                var px;
                var py;
                px = mx * tileSize;
                py = my * tileSize;
    
                var enemyInst;
                enemyInst = instance_create(px, py, obj_rattlesnake);
    
                // Store in global.enemies as a growing array
                var enemyIndex;
                enemyIndex = global.enemyCounter;
                global.enemies[enemyIndex] = enemyInst;
                global.enemyCounter++;
            }
        }
    }
}
