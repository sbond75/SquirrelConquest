var move_speed;
move_speed = 1

if keyboard_check(ord("W")) {
    tileY -= move_speed
}
if keyboard_check(ord("A")) {
    tileX -= move_speed
}
if keyboard_check(ord("S")) {
    tileY += move_speed
}
if keyboard_check(ord("D")) {
    tileX += move_speed
}

var mytile;
mytile = get_tile(tileX, tileY)

// Check for branches
if keyboard_check_pressed(vk_space) && mytile == global.tileTBranch {
    onBranch = !onBranch
}

// Check for walking over acorn
if mytile == 1 and !acorn {
    // Auto pick up
    acorn = true
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
