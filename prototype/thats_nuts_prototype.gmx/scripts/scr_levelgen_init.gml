// Place grouped branch tiles

// n times, pick random position. Then place branches around it in a circle "kernel".

// RNG is inclusive of end and begin here and should use integers only (for langjamgamejam)

var i;
var n;
n = 20; // change as needed

var kernelRadius;
kernelRadius = 2; // radius in tiles

var centerX;
var centerY;
centerX = global.mapWidth / 2;
centerY = global.mapHeight / 2;

var excludeRadius;
excludeRadius = 5;
var excludeRadiusSq;
excludeRadiusSq = excludeRadius * excludeRadius;

var excludeRadiusAcorn;
excludeRadiusAcorn = 1;
var excludeRadiusSqAcorn;
excludeRadiusSqAcorn = excludeRadiusAcorn * excludeRadiusAcorn;

var tileSize;
tileSize = global.tileSize; // tile size in pixels

// Enemy/acorn placement //
var mx;
var my;
var branchNutChance;
var randomNutChance;
var branchEnemyChance;
var randomEnemyChance;
var lim;
// //

for (i = 0; i < n; i += 1)
{
    var tileX;
    tileX = irandom(global.mapWidth);

    var tileY;
    tileY = irandom(global.mapHeight);
    
    // Skip if center is inside the excluded center kernel
    var cdx;
    var cdy;
    cdx = tileX - centerX;
    cdy = tileY - centerY;

    if (cdx * cdx + cdy * cdy <= excludeRadiusSq)
    {
        continue;
    }

    // Center tile (grouped branch "root")
    set_tile(tileX, tileY, 3);

    // Place surrounding branches in a circular-ish kernel
    var dx;
    var dy;

    for (dy = -kernelRadius; dy <= kernelRadius; dy += 1)
    {
        for (dx = -kernelRadius; dx <= kernelRadius; dx += 1)
        {
            // Skip center; already placed
            if (dx == 0 && dy == 0)
            {
                continue;
            }

            // Circle test (integer math)
            if ((dx * dx + dy * dy) <= (kernelRadius * kernelRadius))
            {
                var x2;
                x2 = tileX + dx;

                var y2;
                y2 = tileY + dy;

                // Bounds check
                if (x2 >= 0 && x2 <= global.mapWidth && y2 >= 0 && y2 <= global.mapHeight)
                {
                    // Optional: don't overwrite existing non-empty tiles
                    // if (get_tile(x2, y2) == 0)
                    // {
                    //     set_tile(x2, y2, 3);
                    // }

                    // Simple: just place branch tile
                    set_tile(x2, y2, global.tileTBranch);
                }
            }
        }
    }
}



// -----------------------------------------------------------------------------
// 2. Place acorns
//    - Lower chance on branch tiles
//    - Higher chance elsewhere on the map
// -----------------------------------------------------------------------------

// "On branches" less likely than "random around map"
branchNutChance = 0.01;  // 1% per branch tile
randomNutChance = 0.02;  // 2% per non-branch tile

lim = global.numNuts
while global.nutCounter < lim {
    for (my = 0; my < global.mapHeight; my += 1)
    {
        for (mx = 0; mx < global.mapWidth; mx += 1)
        {
            var tileType;
            tileType = get_tile(mx, my);
    
            var chance;
            chance = 0;
    
            if (tileType == global.tileTBranch)
            {
                chance = branchNutChance;
            }
            else
            {
                chance = randomNutChance;
            }
            
            // Check limit
            if global.nutCounter >= lim {
                break;
            }
            
            // Skip if center is inside the excluded center kernel
            var cdx;
            var cdy;
            cdx = mx - centerX;
            cdy = my - centerY;
            if (cdx * cdx + cdy * cdy <= excludeRadiusSqAcorn)
            {
                continue;
            }
    
            if (chance > 0 && random(1) < chance)
            {
                // Create acorn aligned to tile
                var px;
                var py;
                px = mx * tileSize;
                py = my * tileSize;
    
                var nutInst;
                nutInst = instance_create(px, py, obj_nut);
    
                // Store in global.nuts as a growing array
                var nutIndex;
                nutIndex = global.nutCounter;
                global.nuts[nutIndex] = nutInst;
                global.nutCounter++;
            }
        }
    }
}

// -----------------------------------------------------------------------------
// 3. Place rattlesnakes
//    - No chance on branch tiles
//    - Chance elsewhere on the map
// -----------------------------------------------------------------------------

randomEnemyChance = 0.015;  // 1.5% per non-branch tile

lim = 2 * global.numEnemies div 4
while global.enemyCounter < lim {
    for (my = 0; my < global.mapHeight; my += 1)
    {
        for (mx = 0; mx < global.mapWidth; mx += 1)
        {
            var tileType;
            tileType = get_tile(mx, my);
    
            var chance;
            chance = 0;
    
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

// -----------------------------------------------------------------------------
// 3. Place eagles
//    - Higher chance on branch tiles
//    - Lower elsewhere on the map
// -----------------------------------------------------------------------------

// "On branches" more likely than "random around map"
branchEnemyChance = 0.02;  // 2% per branch tile
randomEnemyChance = 0.01;  // 1% per non-branch tile

lim = 4 * global.numEnemies div 4
while global.enemyCounter < lim {
    for (my = 0; my < global.mapHeight; my += 1)
    {
        for (mx = 0; mx < global.mapWidth; mx += 1)
        {
            var tileType;
            tileType = get_tile(mx, my);
    
            var chance;
            chance = 0;
    
            if (tileType == global.tileTBranch)
            {
                chance = branchEnemyChance;
            }
            else
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
                enemyInst = instance_create(px, py, obj_eagle);
    
                // Store in global.enemies as a growing array
                var enemyIndex;
                enemyIndex = global.enemyCounter;
                global.enemies[enemyIndex] = enemyInst;
                global.enemyCounter++;
            }
        }
    }
}
