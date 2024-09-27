import tseslint from "typescript-eslint";
import tsplugin from "@typescript-eslint/eslint-plugin";
import parser from "@typescript-eslint/parser";

export default tseslint.config(tseslint.configs.eslintRecommended, {
  languageOptions: {
    parser: parser,
    parserOptions: {
      projectService: true,
    },
  },
  ignores: ["node_modules/", "eslint.config.mjs"],
  plugins: { "@typescript-eslint": tsplugin },
  rules: {
    "prettier/prettier": "off",
    "prefer-const": "warn",
    "@typescript-eslint/ban-ts-comment": "warn",
    "no-array-constructor": "off",
    "@typescript-eslint/no-array-constructor": "warn",
    "@typescript-eslint/no-duplicate-enum-values": "warn",
    "@typescript-eslint/no-empty-object-type": "warn",
    "@typescript-eslint/no-explicit-any": "warn",
    "@typescript-eslint/no-extra-non-null-assertion": "warn",
    "@typescript-eslint/no-misused-new": "warn",
    "@typescript-eslint/no-namespace": "warn",
    "@typescript-eslint/no-non-null-asserted-optional-chain": "warn",
    "@typescript-eslint/no-require-imports": "warn",
    "@typescript-eslint/no-this-alias": "warn",
    "@typescript-eslint/no-unnecessary-type-constraint": "warn",
    "@typescript-eslint/no-unsafe-declaration-merging": "warn",
    "@typescript-eslint/no-unsafe-function-type": "warn",
    "@typescript-eslint/strict-boolean-expressions": ["warn"],
    "no-unused-expressions": "off",
    "@typescript-eslint/no-unused-expressions": "warn",
    "no-unused-vars": "off",
    "@typescript-eslint/no-unused-vars": ["warn", { argsIgnorePattern: "^_" }],
    "@typescript-eslint/no-wrapper-object-types": "warn",
    "@typescript-eslint/prefer-as-const": "warn",
    "@typescript-eslint/prefer-namespace-keyword": "warn",
    "@typescript-eslint/triple-slash-reference": "warn",
  },
});
