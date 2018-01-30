import * as fs from 'fs';


export async function writeFile(filename: string, content: string) {
    return new Promise<void>((resolve, reject) => {
        fs.writeFile(filename, content, { encoding: 'utf-8' }, (err) => {
            if (err) {
                reject(err)
            } else {
                resolve()
            }
        })
    })
}

export async function readFile(filename: string): Promise<string> {
    return new Promise<string>((resolve, reject) => {
        fs.readFile(filename, { encoding: 'utf-8'}, (err, data) => {
            if (err) {
                reject(err)
            } else {
                resolve(data)
            }
        })
    })
}