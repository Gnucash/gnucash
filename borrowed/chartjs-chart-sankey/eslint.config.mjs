import { FlatCompat } from '@eslint/eslintrc'
import js from '@eslint/js'
import markdown from '@eslint/markdown'
import tsParser from '@typescript-eslint/parser'
import prettier from 'eslint-plugin-prettier'
import simpleImportSort from 'eslint-plugin-simple-import-sort'
import unusedImports from 'eslint-plugin-unused-imports'
import globals from 'globals'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)
const compat = new FlatCompat({
  baseDirectory: __dirname,
  recommendedConfig: js.configs.recommended,
  allConfig: js.configs.all,
})

export default [
  {
    ignores: ['**/*\\{.,-}min.js', 'dist/**/*'],
  },
  ...compat.extends('chartjs', 'eslint:recommended', 'plugin:@typescript-eslint/recommended', 'prettier'),
  {
    plugins: {
      'unused-imports': unusedImports,
      'simple-import-sort': simpleImportSort,
      prettier,
    },

    languageOptions: {
      globals: {
        ...globals.browser,
        ...globals.node,
        ...globals.jasmine,
      },

      ecmaVersion: 'latest',
      sourceType: 'module',

      parser: tsParser,
    },

    rules: {
      'prettier/prettier': 'error',
      'class-methods-use-this': 'off',
      complexity: ['warn', 10],
      'max-statements': ['warn', 30],
      'no-empty-function': 'off',
      semi: ['error', 'never'],
      quotes: [
        'error',
        'single',
        {
          avoidEscape: true,
          allowTemplateLiterals: true,
        },
      ],
      'comma-spacing': [
        'error',
        {
          before: false,
          after: true,
        },
      ],

      'no-use-before-define': [
        'error',
        {
          functions: false,
        },
      ],
      '@typescript-eslint/no-explicit-any': 'off',
      '@typescript-eslint/no-unused-vars': 'off',
      '@typescript-eslint/indent': 'off',
      'simple-import-sort/imports': [
        'error',
        {
          groups: [
            ['^@?\\w.*\\u0000$', '^[^.].*\\u0000$', '^\\..*\\u0000$'],
            ['^react', '^@?\\w'],
            ['^(@|components)(/.*|$)'],
            ['^\\u0000'],
            ['^\\.\\.(?!/?$)', '^\\.\\./?$'],
            ['^\\./(?=.*/)(?!/?$)', '^\\.(?!/?$)', '^\\./?$'],
            ['^.+\\.?(css)$'],
          ],
        },
      ],

      'unused-imports/no-unused-imports': 'error',
    },
  },
  {
    files: ['src/**/*.ts', '**/*.js', '**/*.mjs'],
  },
  {
    files: ['**/*.md'],
    language: 'markdown/commonmark',
    plugins: {
      markdown,
    },
    rules: {
      'no-irregular-whitespace': 'off',
    },
  },
  {
    files: ['types/**/*.ts'],
    rules: {
      'no-use-before-define': 'warn',
    },
  },
  {
    files: ['**/*.cjs'],
    rules: {
      '@typescript-eslint/no-require-imports': 'off',
    },
  },
]
