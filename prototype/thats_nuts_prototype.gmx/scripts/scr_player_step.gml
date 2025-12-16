var moveSpeed;
moveSpeed = 1

move_player(moveSpeed)

//////////////

// Check wrap
if tileX < 0 {
    tileX = 0
}
if tileX >= global.mapWidth {
    tileX = global.mapWidth - 1
}
if tileY < 0 {
    tileY = 0
}
if tileY >= global.mapHeight {
    tileY = global.mapHeight - 1
}

//////////////

var mytile;
mytile = get_tile(tileX, tileY)

// Check for branches
if keyboard_check_pressed(vk_space) && mytile == global.tileTBranch {
    onBranch = !onBranch
}

// Check for walking over acorn
// Look for an acorn instance exactly on this tile
var nut;
nut = collision_rectangle(
    bbox_left,
    bbox_top,
    bbox_right,
    bbox_bottom,
    obj_nut,
    false,  // precise = false (bbox is fine and cheaper)
    false   // notme = false (player is not obj_nut anyway)
);
if nut != noone and !acorn {
    // Auto pick up
    acorn = true
    
    // Destroy the acorn
    acorn_destroy(nut)
}

// Check for acorn bury or dig up
if keyboard_check_pressed(ord("E")) {
    if !onBranch and acorn and mytile == global.tileTAir {
        // Bury
        stateFrames = 5
    }
    else if !onBranch and !acorn and mytile == global.tileTNut {
        // Dig up acorn
        stateFrames = 5
    }
    //else if !onBranch and acorn {
    //    // Drop acorn onto ground        
    //}
    //else if onBranch and acorn {
    //    // Drop acorn on enemy
    //}
}



//////////

// Apply movement to sprite/obj
x = tileX * global.tileSize
y = tileY * global.tileSize
