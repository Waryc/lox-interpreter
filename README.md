# Lox Interpreter

Lox是一个用Rust编写的解释器，旨在帮助学习编程语言的构建和实现。该项目基于《Crafting Interpreters》一书的内容，提供了一个简单的编程语言Lox的实现。

## 特性

- **词法分析器**：将源代码转换为标记。
- **语法分析器**：将标记转换为抽象语法树（AST）。
- **解释器**：执行AST并返回结果。
- **错误处理**：提供详细的错误信息以帮助调试。

## 目录结构

```
lox/
├── Cargo.toml
├── Cargo.lock
├── src/
│   ├── main.rs
│   ├── cli.rs
│   ├── lexer.rs
│   ├── parser.rs
│   ├── interpreter.rs
│   ├── error.rs
│   ├── ast.rs
│   ├── value.rs
│   └── environment.rs
└── tests/
```

## 安装与运行

1. 确保已安装Rust和Cargo。
2. 克隆该仓库：
   ```bash
   git clone <repository-url>
   ```
3. 进入项目目录并运行：
   ```bash
   cargo run
   ```

## 贡献

欢迎任何形式的贡献！请提交问题或拉取请求。

## 许可证

该项目采用MIT许可证，详情请参见LICENSE文件。
