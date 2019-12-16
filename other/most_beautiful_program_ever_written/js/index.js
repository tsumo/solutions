const { matches } = require('z')

const test = (expr, expectation) => {
    console.log('TESTING', expr)
    let res = eval_expr(expr, x => {throw `Lookup error: ${x} not bound`})
    console.log('RESULT', res)
    if (res !== expectation) {
        console.error('=== TEST FAILED!!! ===')
    }
    console.log()
}

const log = (l, ...xs) => {
    console.log(' '.repeat(l), ...xs)
}

const eval_expr = (expr, env, l = 0) => {
    return matches(expr) (
        (n = Number) => n,
        (f = 'inc', [e]) => {
            log(l, 'Incrementing', e);
            return eval_expr(e, env, l+1) + 1
        },
        (f = 'dec', [e]) => {
            log(l, 'Decrementing', e)
            return eval_expr(e, env, l+1) - 1
        },
        (f = 'is_zero', [e]) => {
            log(l, 'Testing if', e, 'is zero')
            return eval_expr(e, env, l+1) === 0
        },
        (f = 'mult', e1, [e2]) => {
            log(l, 'Multiplying', e1, 'and', e2)
            return eval_expr(e1, env, l+1) * eval_expr(e2, env, l+1)
        },
        (f = 'if', t, c, [a]) => {
            log(l, 'If', t, 'then', c, 'else', a)
            if (eval_expr(t, env, l+1)) {
                return eval_expr(c, env, l+1)
            } else {
                return eval_expr(a, env, l+1)
            }
        },
        (x = String) => {
            log(l, 'Lookup of', x)
            return env(x)
        },
        (f = 'lambda', x, [body]) => {
            log(l, 'Lambda with body', body)
            return arg => eval_expr(body, y => x === y ? arg : env(y), l+1)
        },
        (rator, [rand]) => {
            log(l, 'Application of', rator, 'to', rand)
            return eval_expr(rator, env, l+1)(eval_expr(rand, env, l+1))
        },
    )
}

test(4, 4)
test(['inc', 4], 5)
test(['inc', ['inc', 4]], 6)
test(['dec', 7], 6)
test(['is_zero', ['dec', 1]], true)
test(['mult', 4, ['inc', 2]], 12) // Application of multiple arguments
test(['if', ['is_zero', 0], ['inc', 10], ['dec', 10]], 11) // Conditional evaluation
test([['lambda', 'x', 'x'], 3], 3) // Identity function
test([[['lambda', 'x', 'x'], ['lambda', 'y', 'y']], 3], 3) // Return identity function from lambda, apply it to number
test([['lambda', 'x', ['inc', 'x']], 3], 4) // Apply lambda function
test([['lambda', 'x', ['mult', 2, 'x']], 7], 14) // Doubling function

// Factorial of 5. Recursive definition using Y-combinator
test(
    [[['lambda', '!',
        ['lambda', 'n',
            [['!', '!'], 'n']]],
        ['lambda', '!',
            ['lambda', 'n',
                ['if', ['is_zero', 'n'],
                    1,
                    ['mult', 'n', [['!', '!'], ['dec', 'n']]]]]]],
        5],
    120
)

