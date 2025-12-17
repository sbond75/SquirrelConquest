var enemy;
enemy = argument0

if enemy.canDamagePlayer {
    obj_squirrel.hp -= 1
    if obj_squirrel.hp <= 0 {
        game_over();
    }
    enemy.damagedPlayerFrameCounter = 0
    enemy.canDamagePlayer = false
}
