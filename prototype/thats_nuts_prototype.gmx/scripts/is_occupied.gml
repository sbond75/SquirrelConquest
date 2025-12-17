var tileX;
tileX = argument0
var tileY;
tileY = argument1
var allowBranch;
allowBranch = argument2

var tileSize;
tileSize = global.tileSize;

var bbox_left1;
var bbox_top1;
var bbox_right1;
var bbox_bottom1;
bbox_left1 = tileSize * tileX
bbox_bottom1 = tileSize * tileY
bbox_right1 = bbox_left1 + tileSize
bbox_top1 = bbox_bottom1 + tileSize

while true {  
    var tileType;
    tileType = get_tile(tileX, tileY);
    
    if (tileType == global.tileTTrunk) {
        out_isOccupied = true
        break
    }
    if (!allowBranch and tileType == global.tileTBranch) {
        out_isOccupied = true
        break
    }
    
    // Skip if acorn or enemy there
    var nut;
    nut = collision_rectangle(
        bbox_left1,
        bbox_top1,
        bbox_right1,
        bbox_bottom1,
        obj_nut,
        false,  // precise = false (bbox is fine and cheaper)
        false   // notme = false (player is not obj_nut anyway)
    );
    if nut != noone {
        out_isOccupied = true
        break
    }
    var enemy;
    enemy = collision_rectangle(
        bbox_left1,
        bbox_top1,
        bbox_right1,
        bbox_bottom1,
        obj_rattlesnake,
        false,  // precise = false (bbox is fine and cheaper)
        false   // notme = false (player is not obj_nut anyway)
    );
    if enemy != noone {
        out_isOccupied = true
        break
    }
    enemy = collision_rectangle(
        bbox_left1,
        bbox_top1,
        bbox_right1,
        bbox_bottom1,
        obj_eagle,
        false,  // precise = false (bbox is fine and cheaper)
        false   // notme = false (player is not obj_nut anyway)
    );
    if enemy != noone {
        out_isOccupied = true
        break
    }
    
    out_isOccupied = false
    break
}
