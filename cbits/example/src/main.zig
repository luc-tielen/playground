const std = @import("std");
const testing = std.testing;

export fn add(a: i32, b: i32) i32 {
    return a + b;
}

const Example = struct {
    field: i32,

    fn init(self: *Example) void {
        self.field = 42;
    }

    // Normally: the convention is:
    // fn init() Example {
    //     return Example{ .field = 42 };
    // }

    fn deinit(self: *Example) void {
        _ = self;
    }

    fn do_stuff(self: *Example, arg: i32) bool {
        _ = self;
        return arg == 100;
    }
};

// TODO: mention in blogpost we need to expose a C like API
// C ABI = lowest common denominator

export fn init_example() *Example {
    const obj = std.heap.c_allocator.create(Example) catch std.debug.panic("Failed to allocate Example struct", .{});
    //obj.field = 100;
    obj.init(); // TODO: rename -> create
    return obj;
}

export fn deinit_example(obj: *Example) void {
    std.debug.assert(obj != @intToPtr(*allowzero Example, 0));

    obj.deinit(); // TODO: rename destroy?
    std.heap.c_allocator.destroy(obj);
}

export fn do_stuff_example(obj: *Example, arg: i32) bool {
    std.debug.assert(obj != @intToPtr(*allowzero Example, 0));
    return obj.do_stuff(arg);
}

// TODO: mention comptime, iterators for looping

test "basic add functionality" {
    try testing.expect(add(3, 7) == 10);
}
