if !onBranch && !acorn {
    image_index = 0
}
else if !onBranch && acorn {
    image_index = 1
}
else if onBranch && !acorn {
    image_index = 2
}
else if onBranch && acorn {
    image_index = 3
}

draw_sprite(sprite_index, image_index, x, y)
