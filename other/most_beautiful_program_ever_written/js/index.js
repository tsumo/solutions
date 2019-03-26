var util = require('util')
const { matches } = require('z')

const format = (expr) => {
    res = {func: expr[0]}
    res.arg1 = expr[1]
    if (expr.length >= 3) {
        res.arg2 = expr[2]
    }
    if (expr.length >= 4) {
        res.arg3 = expr[3]
    }
    return res
}
// console.log(format( ['if', 0, 1, 2] ))

const test = (expr, expectation) => {
    console.log(util.inspect(expr, false, null))
    let res = eval_expr(expr, (x) => {throw `Lookup error: ${x} not bound`})
    console.log('=>', res)
    if (res !== expectation) {
        console.log('=== TEST FAILED!!! ===')
    }
    console.log()
}

const eval_expr = (expr, env) => {
    return matches(expr) (
        (n = Number) => n,
        (e = {func: 'inc'}) => eval_expr(e.arg1, env) + 1,
        (e = {func: 'dec'}) => eval_expr(e.arg1, env) - 1,
        (e = {func: 'is_zero'}) => eval_expr(e.arg1, env) === 0,
        (e = {func: 'mult'}) => eval_expr(e.arg1, env) * eval_expr(e.arg2, env),
        (e = {func: 'if'}) => {
            if (eval_expr(e.arg1, env)) {
                return eval_expr(e.arg2, env)
            } else {
                return eval_expr(e.arg3, env)
            }
        },
        (x = String) => env(x),
        (e = {func: 'lambda'}) => (arg) => (eval_expr(e.arg2), (y) => e.arg1 == y ? arg : env(y)),
        (e = Object) => eval_expr(e.func, env).apply(eval_expr(e.arg1, env)),
    )
}

test(4, 4)
test({ func: 'inc', arg1: 4 }, 5)
test({
    func: 'inc',
    arg1: { func: 'inc', arg1: 4 },
}, 6)
test({
    func: 'dec',
    arg1: 7,
}, 6)
test({
    func: 'mult',
    arg1: 4,
    arg2: { func: 'inc', arg1: 3 },
}, 16)
test({
    func: 'is_zero',
    arg1: { func: 'dec', arg1: 1 },
}, true)
test({
    func: 'if',
    arg1: { func: 'is_zero', arg1: 0 },
    arg2: { func: 'inc', arg1: 10 },
    arg3: { func: 'dec', arg1: 10 },
}, 11)
test({
    func: 'lambda',
    arg1: 'x',
    arg2: {
        func: 'inc',
        arg1: 2,
    },
})
const a = {
    func: 'lambda',
    arg1: 'x',
    arg2: {
        func: 'inc',
        arg1: 2,
    },
}
const res = eval_expr(a, (x) => {throw `Lookup error: ${x} not bound`})
console.log(res(0))
