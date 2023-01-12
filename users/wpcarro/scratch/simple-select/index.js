// const input = '-director:"Von \\"Fluke\\" Neumann" foo:/this is a test/ (director:"Tarantino" OR director:"Scorsese")';
const input = 'director:"Tarantino" OR director:"Scorsese"';

const state = {
    // Match values case sensitively when filtering.
    caseSensitive: false,
    // Coerce values into regular expressions (instead of strings) when they're defined as atoms.
    preferRegex: false,
};

// TODO(wpcarro): Support global queries like "Tarantino" that try to match against all columns in a row.
// TODO(wpcarro): Support filtering by date (before, after).

function select(query, xs) {
    const predicate = compile(parse(query));
    return xs.filter(predicate);
}

function compile(ast) {
    if (ast.type === 'CONJUNCTION') {
        const lhs = compile(ast.lhs);
        const rhs = compile(ast.rhs);

        if (ast.joint === 'AND') {
            return function(x) {
                return lhs(x) && rhs(x);
            };
        }
        if (ast.joint === 'OR') {
            return function(x) {
                return lhs(x) || rhs(x);
            };
        }
    }
    if (ast.type === 'SELECTION') {
        const f = compile(ast.val);
        return function(row) {
            return ast.negate ? f(row[ast.key]) : f(row[ast.key]);
        };
    }
    if (ast.type === 'STRING') {
        return function(x) {
            if (state.caseSensitive) {
                return x === ast.val;
            } else {
                return x.toLowerCase() === ast.val.toLowerCase();
            }
        };
    }
    if (ast.type === 'REGEX') {
        return function(x) {
            return ast.val.test(x);
        };
    }
}

// A "selection" without a "$column:" prefix should fuzzy-search all columns.
//
// conjunction -> selection ( ( "AND" | "OR" )? selection )* ;
// selection   -> "-"? COLUMN ":" ( regex | string ) | regex ;
// regex       -> [_-a-zA-Z0-9] | "/" [ _-a-zA-Z0-9] "/" | string ;
// string      -> "\"" [ _-a-zA-Z0-9] "\"" ;

// Whatever characters are valid for a JS regex.
const ATOM_REGEX = /[-_.\[\]a-zA-Z0-9*+^$]/;

function tokenize(x) {
    const result = [];
    let i = 0;
    while (i < x.length) {
        if (x[i] === ' ') {
            i += 1;
            while (i < x.length && x[i] === ' ') {
                i += 1;
            }
            result.push(['WHITESPACE', null]);
            continue;
        }
        if (x[i] === '-') {
            result.push(['NEGATE', null]);
            i += 1;
            continue;
        }
        if (ATOM_REGEX.test(x[i])) {
            let curr = x[i];
            i += 1;
            while (i < x.length && ATOM_REGEX.test(x[i])) {
                curr += x[i];
                i += 1;
            }
            result.push(['ATOM', curr]);
            continue;
        }
        if (x[i] === ':') {
            result.push(['COLON', null]);
            i += 1;
            continue;
        }
        if (x[i] === '(') {
            result.push(['LPAREN', null]);
            i += 1;
            continue;
        }
        if (x[i] === ')') {
            result.push(['RPAREN', null]);
            i += 1;
            continue;
        }
        if (x[i] === '/') {
            let start = i;
            let curr = '';
            i += 1;
            while (i < x.length && x[i] !== '/') {
                curr += x[i];
                i += 1;
            }
            // error
            if (i >= x.length) {
                throw `Tokenize Error: EOL while attempting to tokenize the regex beginning at column: ${start}`;
            }
            if (x[i] === '/') {
                result.push(['REGEX', curr]);
                i += 1;
            }
            continue;
        }
        if (x[i] === '"') {
            let start = i;
            let curr = '';
            i += 1;
            while (i < x.length && x[i] !== '"') {
                // continue on \"
                if (x[i] === '\\' && x[i + 1] === '"') {
                    curr += '\"';
                    i += 2;
                } else {
                    curr += x[i];
                    i += 1;
                }
            }
            if (i >= x.length) {
                throw `Tokenize Error: EOL while attempting to tokenize the string starting at column: ${start}`;
            }
            if (x[i] === '"') {
                result.push(['STRING', curr]);
                i += 1;
            }
            continue;
        }
        else {
            i += 1;
        }
    }
    return result;
}

function expect(f, expectation, p) {
    const [type, val] = p.tokens[p.i];
    if (f(type, val)) {
        p.i += 1;
    } else {
        throw `Parse Error: expected ${expectation}, but got ${p.tokens[p.i]}; ${JSON.stringify(p)}`
    }
}

function matches(f, p) {
    const [type, val] = p.tokens[p.i];
    if (f(type, val)) {
        return true;
    }
    return false;
}

function match(f, expectation, p) {
    const [type, val] = p.tokens[p.i];
    if (f(type, val)) {
        p.i += 1;
        return val;
    }
    throw `Parse Error: expected ${expectation}, but got: ${p.tokens[p.i]}; ${JSON.stringify(p)}`;
}

function skipWhitespace(p) {
    while (p.i < p.tokens.length && matches((type, _) => type === 'WHITESPACE', p)) {
        p.i += 1;
    }
}

function parser(tokens) {
    return { i: 0, tokens };
}

function parse(x) {
    const tokens = tokenize(x);
    const p = parser(tokens);
    return conjunction(p);
}

function conjunction(p) {
    skipWhitespace(p);

    const lhs = selection(p);
    skipWhitespace(p);

    if (p.i >= p.tokens.length) {
        return lhs;
    }

    let joint = 'AND';
    if (matches((type, val) => type === 'ATOM' && val === 'AND', p)) {
        joint = 'AND';
        p.i += 1;
    } else if (matches((type, val) => type === 'ATOM' && val === 'OR', p)) {
        joint = 'OR';
        p.i += 1;
    }
    skipWhitespace(p);
    let rhs = conjunction(p);

    return {
        type: 'CONJUNCTION',
        joint,
        lhs,
        rhs,
    };
}

function selection(p) {
    // column:value OR -column:value
    if (matches((type, _) => type === 'ATOM' || type === 'NEGATE', p)) {
        let negate = false;
        if (p.tokens[p.i][0] === 'NEGATE') {
            negate = true;
            p.i += 1;
        }
        const key = match((type, _) => type === 'ATOM', 'a column label', p);
        expect((type, val) => type === 'COLON', 'a colon', p);
        const val = value(p);
        return {
            type: 'SELECTION',
            negate,
            key,
            val,
        };
    } else {
        return value(p);
    }
}

function value(p) {
    const [type, val] = p.tokens[p.i];

    // Cast atoms into strings or regexes depending on the current state.
    if (type === 'ATOM') {
        p.i += 1;
        if (state.preferRegex) {
            const regex = state.caseSensitive ? new RegExp(val) : new RegExp(val, "i");
            return { type: 'REGEX', val: regex };
        } else {
            return { type: 'STRING', val }
        }
    }
    if (type === 'STRING') {
        p.i += 1;
        return { type, val };
    }
    if (type === 'REGEX') {
        p.i += 1;
        const regex = state.caseSensitive ? new RegExp(val) : new RegExp(val, "i");
        return { type, val: regex };
    }
    throw `Parse Error: Expected a regular expression or a string, but got: ${p.tokens[p.i]}; ${JSON.stringify(p)}`;
}
