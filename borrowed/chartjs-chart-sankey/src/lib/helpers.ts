export const defined = <T>(x?: T): x is T => x !== undefined

export function toTextLines(raw?: string | number | Array<string | number | Array<string | number>>): string[] {
  if (!raw) return []

  const lines: string[] = []
  const inputs = Array.isArray(raw) ? raw : [raw]

  while (inputs.length) {
    const input = inputs.pop()
    if (typeof input === 'string') {
      lines.unshift(...input.split('\n'))
    } else if (Array.isArray(input)) {
      inputs.push(...input)
    } else if (input) {
      lines.unshift(`${input}`)
    }
  }

  return lines
}

export function validateSizeValue(size: any): 'min' | 'max' {
  if (!size || ['min', 'max'].indexOf(size) === -1) {
    return 'max'
  }
  return size
}
