const std = @import("std");
const build = @import("../build.zig.zon");

const config = @import("config");

pub fn main() !void {
    const alloc = std.heap.smp_allocator;

    const ctx = try parseCtx(alloc);
    defer ctx.deinit();

    const argv = try ctx.getArgv(alloc);
    defer alloc.free(argv);

    const flattened = try flatten(alloc, argv);
    defer alloc.free(flattened);

    // enter alt buffer
    printLog("\x1b[?1049h", .{});

    const exitcode = runProgram(ctx) catch {
        panicWithErr("Failed to run program '{s}'", .{flattened});
    };
    // Enter again in case of the input program exits
    printLog("\x1b[?1049h", .{});

    if (ctx.time != 0 or ctx.enter)
        wait(ctx, exitcode);

    exit(exitcode);
}

pub fn runProgram(ctx: Ctx) !u8 {
    const alloc = std.heap.smp_allocator;

    const act = std.os.linux.Sigaction{
        .handler = .{ .handler = sigintHandler },
        .mask = std.os.linux.empty_sigset,
        .flags = 0,
    };

    var env_map = try ctx.getEnvMap(alloc);
    defer env_map.deinit();

    const argv = try ctx.getArgv(alloc);
    defer alloc.free(argv);

    var child = std.process.Child.init(argv, alloc);
    child.env_map = &env_map;

    try child.spawn();

    if (std.os.linux.sigaction(std.os.linux.SIG.INT, &act, null) != 0) {
        _ = child.kill() catch {};
    }

    const flattened = try flatten(alloc, argv);
    defer alloc.free(flattened);

    // Move cursor to 0, 0
    printLog("\x1b[H", .{});

    const term = child.wait() catch {
        printLog("\x1b[?1049l", .{});
        panicWithErr("Failed to execute program: '{s}'\n", .{flattened});
    };

    return switch (term) {
        .Exited => |c| c,
        .Signal => |c| @truncate(c),
        .Stopped => |c| @truncate(c),
        .Unknown => |c| @truncate(c),
    };
}

fn wait(ctx: Ctx, code: u8) void {
    const name = ctx.program orelse "null";
    if (code == 0) {
        printLog(blue("\nProgram '{s}' finished\n"), .{name});
    } else {
        printLog(red("\nProgram '{s}' exited with error code {}\n"), .{ name, code });
    }

    if (ctx.enter) {
        _ = std.io.getStdIn().reader().readByte() catch {};
    } else {
        std.Thread.sleep(std.time.ns_per_ms * ctx.time);
    }
}

fn sigintHandler(sig: c_int) callconv(.C) void {
    _ = sig;
}

pub fn flatten(allocator: std.mem.Allocator, nested: []const []const u8) std.mem.Allocator.Error![]const u8 {
    var list = std.ArrayList(u8).init(allocator);
    defer list.deinit();

    for (nested) |slice| {
        try list.appendSlice(slice);
        try list.append(' ');
    }
    _ = list.pop();

    return try list.toOwnedSlice();
}

pub fn red(comptime str: []const u8) []const u8 {
    return "\x1b[31m" ++ str ++ "\x1b[0m";
}

pub fn blue(comptime str: []const u8) []const u8 {
    return "\x1b[34m" ++ str ++ "\x1b[0m";
}

pub fn printHelp(arg0: []const u8) void {
    std.io.getStdOut().writer().print(
        \\{s} v{s}
        \\Execute programs in the alternate screen buffer
        \\
        \\{s} {s} [options] program [program options]\n"
        \\
        \\{s}
        \\  --no-env            Execute the program with no environment variables
        \\  --no-path           Don't search for the program in $PATH
        \\  -e, --enter         Waits for an enter press to close the alt screen, after the input program has finished
        \\  -t, --time=MS       Set a timer for how long the alt screen should persist, after the input program has finished
        \\  --with-env=K:V,..   Add environment variables that the program will be executed with, in K:V pairs, each pair seperated by a ','
        \\  -v, --version       Prints the current version
        \\  -h, --help          Print the help page
        \\
    , .{ arg0, config.version, blue("USAGE:"), arg0, blue("OPTIONS:") }) catch {};
}

pub fn exit(code: u8) noreturn {
    printLog("\x1b[?1049l", .{});
    std.process.exit(code);
}
pub fn printLog(comptime fmt: []const u8, args: anytype) void {
    std.io.getStdOut().writer().print(fmt, args) catch {};
}
pub fn printErr(comptime fmt: []const u8, args: anytype) void {
    std.io.getStdOut().writer().print(red("[ALTWRAP ERROR]: " ++ fmt), args) catch {};
}
pub fn panicWithErr(comptime fmt: []const u8, args: anytype) noreturn {
    printErr(fmt, args);
    exit(1);
}

const Ctx = struct {
    const Self = @This();

    allocator: std.mem.Allocator,

    program: ?[]const u8 = null,
    progam_opt: []const []const u8 = &.{},
    no_env: bool = false,
    no_path: bool = false,
    enter: bool = false,
    time: u64 = 0,
    with_env: [][2][]const u8 = &.{},

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{ .allocator = allocator };
    }

    pub fn deinit(self: *const Self) void {
        if (self.program) |name| {
            self.allocator.free(name);
        }

        self.allocator.free(self.progam_opt);

        self.allocator.free(self.with_env);
    }

    pub fn getArgv(ctx: Self, allocator: std.mem.Allocator) std.mem.Allocator.Error![]const []const u8 {
        if (ctx.program) |name| {
            const argv = try allocator.alloc([]const u8, ctx.progam_opt.len + 1);
            argv[0] = name;
            @memcpy(argv[1..], ctx.progam_opt);
            return argv;
        } else {
            return &.{};
        }
    }

    pub fn getEnvMap(self: Self, allocator: std.mem.Allocator) std.process.GetEnvMapError!std.process.EnvMap {
        if (self.no_env) {
            return std.process.EnvMap.init(allocator);
        }

        var env_map = try std.process.getEnvMap(allocator);
        for (self.with_env) |set| {
            try env_map.put(set[0], set[1]);
        }
        return env_map;
    }

    pub fn debugPrint(self: Self) void {
        if (self.program) |name| {
            printLog("PROGRAM: {s}\n", .{name});

            printLog("FLAGS: (\n", .{});
            for (self.progam_opt) |opt| {
                printLog("\t{s}\n", .{opt});
            }
            printLog(")\n", .{});
        }
        printLog("NO-ENV: {}\n", .{self.no_env});
        printLog("NO-PATH: {}\n", .{self.no_path});

        printLog("ENV: (\n", .{});
        for (self.with_env) |set| {
            printLog("\t{s} : {s}\n", .{ set[0], set[1] });
        }
        printLog(")\n", .{});
    }
};

pub fn parseCtx(allocator: std.mem.Allocator) std.mem.Allocator.Error!Ctx {
    var args = std.process.argsWithAllocator(allocator) catch {
        panicWithErr("Failed to get args", .{});
    };
    defer args.deinit();

    const name = args.next() orelse "altwrap";

    var ctx: Ctx = .init(allocator);

    var program_opt_list = std.ArrayList([]const u8).init(allocator);
    defer program_opt_list.deinit();

    var errors = std.ArrayList([]const u8).init(allocator);
    defer errors.deinit();

    while (args.next()) |arg| {
        // parse altwrap args until program name is specified
        if (ctx.program == null) {
            if (std.mem.eql(u8, arg, "--no-env")) {
                ctx.no_env = true;
                continue;
            }
            if (std.mem.eql(u8, arg, "--no-path")) {
                ctx.no_path = true;
                continue;
            }
            if (std.mem.eql(u8, arg, "--enter") or std.mem.eql(u8, arg, "-e")) {
                ctx.enter = true;
                continue;
            }
            if (std.mem.startsWith(u8, arg, "--with-env")) {
                var env_list = std.ArrayList([2][]const u8).init(allocator);
                defer env_list.deinit();

                var split_arg = std.mem.splitScalar(u8, arg, '=');

                // Get rid of --with-env
                _ = split_arg.next();

                const sets_raw = split_arg.next() orelse {
                    try errors.append(try allocator.dupe(u8, "--with-env option expected = as seperator"));
                    continue;
                };

                var sets = std.mem.splitScalar(u8, sets_raw, ',');
                if (sets.peek() == null) {
                    try errors.append(try allocator.dupe(u8, "--with-env option expected sets of environments keys and values formatted like =K:V,..."));
                    continue;
                }

                while (sets.next()) |set| {
                    if (set.len == 0) continue;

                    var kv = std.mem.splitScalar(u8, set, ':');
                    const key = kv.first();
                    const val = kv.rest();

                    try env_list.append(.{
                        try allocator.dupe(u8, key),
                        try allocator.dupe(u8, val),
                    });
                }

                ctx.with_env = try env_list.toOwnedSlice();
                continue;
            }
            if (std.mem.startsWith(u8, arg, "--time") or std.mem.startsWith(u8, arg, "-t")) {
                var split_arg = std.mem.splitScalar(u8, arg, '=');

                // Get rid of --timeout / -t
                _ = split_arg.next();

                const num_str = split_arg.next() orelse {
                    try errors.append(try allocator.dupe(u8, "--time option expects a number as time in MS, got nothing"));
                    continue;
                };

                const num = std.fmt.parseInt(u64, num_str, 0) catch {
                    const errmsg = try std.fmt.allocPrint(allocator, "--time option expects a valid number as time in MS, got '{s}'", .{num_str});
                    try errors.append(errmsg);
                    continue;
                };

                ctx.time = num;
                continue;
            }
            if (!std.mem.startsWith(u8, arg, "-")) {
                ctx.program = try allocator.dupe(u8, arg);
                continue;
            }

            if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
                printHelp(name);
                std.process.exit(0);
            }
            if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--version")) {
                printLog("{s} v{s}\n", .{ name, config.version });
                std.process.exit(0);
            }

            // Unreachable state by valid flags
            const err_msg = try std.fmt.allocPrint(allocator, "Unknown option: '{s}'", .{arg});

            try errors.append(err_msg);
        } else {
            try program_opt_list.append(arg);
        }
    }

    if (ctx.program == null) {
        try errors.append(try allocator.dupe(u8, "No program name supplied"));
    }

    ctx.progam_opt = try program_opt_list.toOwnedSlice();

    if (errors.items.len != 0) {
        for (errors.items) |err| {
            printErr("{s}\n", .{err});
            allocator.free(err);
        }
        exit(1);
    }

    return ctx;
}
