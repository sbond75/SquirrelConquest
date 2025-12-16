var nut;
nut = argument0

 // Remove from global.nuts by setting the entry to noone
var i;
for (i = 0; i < global.nutCounter; i += 1)
{
    if (global.nuts[i] == nut)
    {
        global.nuts[i] = noone;
        break;
    }
}

// Destroy the acorn instance
with (nut)
{
    instance_destroy();
}
