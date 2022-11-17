export interface ImportedInterface {
    value: string,
}

export type ImportedRecord1 = Record<string, string>;

export type ImportedArray1 = string[];

export type ImportedRecord2 = Record<string, ImportedInterface>;

export type ImportedArray2 = ImportedInterface[];
