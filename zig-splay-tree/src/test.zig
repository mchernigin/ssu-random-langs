const std = @import("std");

const expect = std.testing.expect;
const allocator = std.testing.allocator;

const st = @import("splay-tree.zig");

test "splay_tree_create" {
    var tree = st.SplayTree(i32).init(std.testing.allocator);
    defer tree.deinit();

    try tree.insert(2);
    try tree.insert(1);
    try tree.insert(3);

    std.debug.print("\n{any}\n", .{tree.root});
}
