import type { Config } from 'jest'

const config: Config = {
  collectCoverage: true,
  coverageProvider: 'v8',
  coverageReporters: ['lcov', 'text'],
  extensionsToTreatAsEsm: ['.ts'],
  roots: ['src'],
  testEnvironment: 'node',
  transform: {
    '^.+.tsx?$': ['ts-jest', { useESM: true }],
  },
}

export default config
