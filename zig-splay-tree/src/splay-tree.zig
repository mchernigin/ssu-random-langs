const std = @import("std");

pub fn SplayTree(comptime T: type) type {
    return struct {
        const Self = @This();

        const Node = struct {
            left: ?*Node,
            right: ?*Node,
            data: T,
        };

        gpa: std.mem.Allocator,
        root: ?*Node,

        pub fn init(gpa: std.mem.Allocator) Self {
            return Self{ .gpa = gpa, .root = null };
        }

        fn deinit_node(self: *Self, node: *Node) void {
            if (node.left != null) {
                self.deinit_node(node.left.?);
            }
            if (node.right != null) {
                self.deinit_node(node.right.?);
            }
            self.gpa.destroy(node);
        }

        pub fn deinit(self: *Self) void {
            if (self.root != null) {
                self.deinit_node(self.root.?);
            }
        }

        pub fn insert(self: *Self, data: T) !void {
            var ptr = &self.root;
            while (ptr.*) |cur_node| {
                if (data < cur_node.data) {
                    ptr = &cur_node.left;
                }
                if (data > cur_node.data) {
                    ptr = &cur_node.right;
                }
            }

            const node = try self.gpa.create(Node);
            node.* = .{
                .data = data,
                .left = null,
                .right = null,
            };
            ptr.* = node;
        }

        fn subtree_height(ptr: ?*Node, cur_height: u16) u16 {
            if (ptr) |node| {
                const left_height = subtree_height(node.left, cur_height + 1);
                const right_height = subtree_height(node.right, cur_height + 1);
                return @max(left_height, right_height);
            } else {
                return cur_height;
            }
        }

        pub fn height(self: *Self) u16 {
            return subtree_height(self.root, 0);
        }

        fn subtree_contains(ptr: ?*Node, value: T) bool {
            if (ptr) |node| {
                if (node.data == value) {
                    return true;
                }
                return subtree_contains(node.left, value) or
                    subtree_contains(node.right, value);
            } else {
                return false;
            }
        }

        pub fn contains(self: *Self, value: T) bool {
            return subtree_contains(self.root, value);
        }
    };
}
