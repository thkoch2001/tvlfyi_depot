import tseslint from 'typescript-eslint';
import tsplugin from '@typescript-eslint/eslint-plugin';
import parser from '@typescript-eslint/parser';

let recommended = { ...tseslint.configs.eslintRecommended };

let set = tseslint.config(recommended, {
  languageOptions: {
    parser: parser,
    parserOptions: {
      projectService: true,
    },
  },
  plugins: { '@typescript-eslint': tsplugin },
  rules: {
    'prettier/prettier': 'off',
    'prefer-const': 'warn',
    '@typescript-eslint/ban-ts-comment': 'warn',
    'no-array-constructor': 'off',
    '@typescript-eslint/no-array-constructor': 'warn',
    '@typescript-eslint/no-duplicate-enum-values': 'warn',
    '@typescript-eslint/no-empty-object-type': 'warn',
    '@typescript-eslint/no-explicit-any': 'warn',
    '@typescript-eslint/no-extra-non-null-assertion': 'warn',
    '@typescript-eslint/no-misused-new': 'warn',
    '@typescript-eslint/no-namespace': 'warn',
    '@typescript-eslint/no-non-null-asserted-optional-chain': 'warn',
    '@typescript-eslint/no-require-imports': 'warn',
    '@typescript-eslint/no-this-alias': 'warn',
    '@typescript-eslint/no-unnecessary-type-constraint': 'warn',
    '@typescript-eslint/no-unsafe-declaration-merging': 'warn',
    '@typescript-eslint/no-unsafe-function-type': 'warn',
    '@typescript-eslint/strict-boolean-expressions': ['warn'],
    'no-unused-expressions': 'off',
    '@typescript-eslint/no-unused-expressions': 'warn',
    'no-unused-vars': 'off',
    '@typescript-eslint/no-unused-vars': ['warn', { argsIgnorePattern: '^_' }],
    '@typescript-eslint/no-wrapper-object-types': 'warn',
    '@typescript-eslint/prefer-as-const': 'warn',
    '@typescript-eslint/prefer-namespace-keyword': 'warn',
    '@typescript-eslint/triple-slash-reference': 'warn',

    '@typescript-eslint/await-thenable': 'warn',
    'no-array-constructor': 'off',
    '@typescript-eslint/no-array-delete': 'warn',
    '@typescript-eslint/no-base-to-string': 'warn',
    '@typescript-eslint/no-duplicate-type-constituents': 'warn',
    '@typescript-eslint/no-floating-promises': 'warn',
    '@typescript-eslint/no-for-in-array': 'warn',
    'no-implied-eval': 'off',
    '@typescript-eslint/no-implied-eval': 'warn',
    '@typescript-eslint/no-misused-promises': 'warn',
    '@typescript-eslint/no-redundant-type-constituents': 'warn',
    '@typescript-eslint/no-unnecessary-type-assertion': 'warn',
    '@typescript-eslint/no-unsafe-argument': 'warn',
    '@typescript-eslint/no-unsafe-assignment': 'warn',
    '@typescript-eslint/no-unsafe-call': 'warn',
    '@typescript-eslint/no-unsafe-enum-comparison': 'warn',
    '@typescript-eslint/no-unsafe-member-access': 'warn',
    '@typescript-eslint/no-unsafe-return': 'warn',
    '@typescript-eslint/no-unsafe-unary-minus': 'warn',
    'no-throw-literal': 'off',
    '@typescript-eslint/only-throw-error': 'warn',
    'prefer-promise-reject-errors': 'off',
    '@typescript-eslint/prefer-promise-reject-errors': 'warn',
    'require-await': 'off',
    '@typescript-eslint/require-await': 'warn',
    '@typescript-eslint/restrict-plus-operands': 'warn',
    '@typescript-eslint/restrict-template-expressions': 'warn',
    '@typescript-eslint/unbound-method': 'warn',
  },
});

// override files for each set
const files = ['src/**/*.ts', 'src/**/*.tsx'];
for (let s of set) {
  s.files = files;
}

export default set;
