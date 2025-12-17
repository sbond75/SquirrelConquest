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

var tileSize;
tileSize = global.tileSize; // tile size in pixels

var branchNutChance;
var randomNutChance;

// "On branches" less likely than "random around map"
branchNutChance = 0.01;  // 1% per branch tile
randomNutChance = 0.02;  // 2% per non-branch tile

var mx;
var my;

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
        if global.nutCounter >= global.numNuts {
            break;
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
