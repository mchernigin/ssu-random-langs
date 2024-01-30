const std = @import("std");

const testing = std.testing;

const st = @import("splay-tree.zig");

test "Simple insertions" {
    var tree = st.SplayTree(i32).init(testing.allocator);
    defer tree.deinit();

    try tree.insert(1);
    try tree.insert(2);
    try tree.insert(3);
}

test "Tree height" {
    var tree = st.SplayTree(i32).init(testing.allocator);
    defer tree.deinit();

    try tree.insert(1);
    try tree.insert(2);
    try tree.insert(3);

    try testing.expectEqual(@as(u16, 3), tree.height());

    try tree.insert(4);
    try tree.insert(5);

    try testing.expectEqual(@as(u16, 5), tree.height());
}

test "Tree contains" {
    var tree = st.SplayTree(i32).init(testing.allocator);
    defer tree.deinit();

    try tree.insert(17);
    try tree.insert(20);
    try tree.insert(14);
    try tree.insert(15);
    try tree.insert(12);
    try tree.insert(13);

    try testing.expectEqual(true, tree.contains(15));
    try testing.expectEqual(true, tree.contains(20));
    try testing.expectEqual(false, tree.contains(16));
    try testing.expectEqual(false, tree.contains(0));
}
