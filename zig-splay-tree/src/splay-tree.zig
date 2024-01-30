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

        pub fn height(self: *Self) u16 {
            _ = self;
        }
    };
}
