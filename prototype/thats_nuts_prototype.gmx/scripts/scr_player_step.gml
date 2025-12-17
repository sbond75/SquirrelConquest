var moveSpeed;
moveSpeed = 1

if stateFrames == 0 {
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
    
    // Apply movement to sprite/obj
    x = tileX * global.tileSize
    y = tileY * global.tileSize
    
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
    if nut != noone and nut.buried == false and !acorn {
        // Auto pick up
        acorn = true
        
        // Destroy the acorn
        acorn_destroy(tileX, tileY, false)
    }
}

// Check for in-progress action processing
if stateFrames > 0 {
    // Update time left
    stateFrames--
    
    if stateFrames == 0 {
        // On finished:
        // Perform the action:
        if acorn && !eating {
            // Burying
            acorn_create(tileX, tileY, true, false, 0, 0, false)
            
            acorn = false
        }
        else if !acorn && !eating {
            // Digging up
            acorn = true
            
            acorn_destroy(tileX, tileY, false)
        }
        else if eating {
            // Eating
            eating = false
            acorn = false
            if hp < 3 {
                hp++
            }
        }
    }
}
// Check for acorn bury or dig up
else if keyboard_check_pressed(ord("E")) && stateFrames == 0 {
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
    
    if !onBranch and acorn and mytile == global.tileTAir and nut == noone {
        // Bury
        stateFrames = 5
    }
    else if !onBranch and !acorn and nut != noone and nut.buried == true {
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
// Check for acorn eat
else if keyboard_check_pressed(ord("F")) && stateFrames == 0 && acorn {
    eating = true
    stateFrames = 5
}
// Check for acorn throw
else if acorn {
    var dx;
    dx=0
    var dy;
    dy=0
    if keyboard_check_pressed(vk_left) {
        dx -= 1;
    }
    if keyboard_check_pressed(vk_right) {
        dx += 1;
    }
    if keyboard_check_pressed(vk_up) {
        dy -= 1;
    }
    if keyboard_check_pressed(vk_down) {
        dy += 1;
    }
    
    if abs(dx)>0 || abs(dy)>0 {
        // Throw
        acorn_throw(self, dx, dy)
    }
}


//////////
