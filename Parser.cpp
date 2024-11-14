#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <fstream>
#include <stdexcept>
using namespace std;

enum TokenType
{
    T_INT,
    T_ID,
    T_NUM,
    T_IF,
    T_ELSE,
    T_RETURN,
    T_ASSIGN,
    T_PLUS,
    T_MINUS,
    T_MUL,
    T_DIV,
    T_LPAREN,
    T_RPAREN,
    T_LBRACE,
    T_RBRACE,
    T_SEMICOLON,
    T_GT,
    T_WHILE,
    T_FUNC,
    T_SWITCH,
    T_CASE,
    T_DEFAULT,
    T_BREAK,
    T_BOOL,
    T_TRUE,
    T_FALSE,
    T_STRING,
    T_COMMENT,
    T_COLON,
    T_EOF,
    T_FLOAT,
    T_DOUBLE,
    T_LT

};

struct Token
{
    TokenType type;
    string value;
    int line;
};

class Lexer
{
private:
    string src;
    size_t pos;
    int line;

public:
    Lexer(const string &src) : src(src), pos(0), line(1) {}

    vector<Token> tokenize()
    {
        vector<Token> tokens;
        while (pos < src.size())
        {
            char current = src[pos];

            if (current == '\n')
            {
                line++;
                pos++;
                continue;
            }
            if (isspace(current))
            {
                pos++;
                continue;
            }
            if (current == '/' && pos + 1 < src.size() && src[pos + 1] == '/')
            {
                while (pos < src.size() && src[pos] != '\n')
                    pos++;
                continue;
            }
            if (isdigit(current))
            {
                tokens.push_back(Token{T_NUM, consumeNumber(), line});
                continue;
            }
            if (isalpha(current)) {
                string word = consumeWord();
                if (word == "int") tokens.push_back(Token{T_INT, word, line});
                else if (word == "float") tokens.push_back(Token{T_FLOAT, word, line});
                else if (word == "double") tokens.push_back(Token{T_DOUBLE, word, line});
                else if (word == "bool") tokens.push_back(Token{T_BOOL, word, line});\
                else if (word == "string") tokens.push_back(Token{T_STRING, word, line});
                else if (word == "true") tokens.push_back(Token{T_TRUE, word, line});
                else if (word == "false") tokens.push_back(Token{T_FALSE, word, line});
                else if (word == "if") tokens.push_back(Token{T_IF, word, line});
                else if (word == "else") tokens.push_back(Token{T_ELSE, word, line});
                else if (word == "return") tokens.push_back(Token{T_RETURN, word, line});
                else if (word == "func") tokens.push_back(Token{T_FUNC, word, line});
                else if (word == "switch") tokens.push_back(Token{T_SWITCH, word, line});
                else if (word == "case") tokens.push_back(Token{T_CASE, word, line});
                else if (word == "default") tokens.push_back(Token{T_DEFAULT, word, line});
                else if (word == "break") tokens.push_back(Token{T_BREAK, word, line});
                else if (word == "while") tokens.push_back(Token{T_WHILE, word, line});
                else tokens.push_back(Token{T_ID, word, line});
                continue;
            }
            if (current == '"')
            {
                tokens.push_back(Token{T_STRING, consumeString(), line});
                continue;
            }

            switch (current)
            {
                case '=': tokens.push_back(Token{T_ASSIGN, "=", line}); break;
                case '+': tokens.push_back(Token{T_PLUS, "+", line}); break;
                case '-': tokens.push_back(Token{T_MINUS, "-", line}); break;
                case '*': tokens.push_back(Token{T_MUL, "*", line}); break;
                case '/': tokens.push_back(Token{T_DIV, "/", line}); break;
                case '(': tokens.push_back(Token{T_LPAREN, "(", line}); break;
                case ')': tokens.push_back(Token{T_RPAREN, ")", line}); break;
                case '{': tokens.push_back(Token{T_LBRACE, "{", line}); break;
                case '}': tokens.push_back(Token{T_RBRACE, "}", line});break;
                case ';': tokens.push_back(Token{T_SEMICOLON, ";", line});break;
                case '>': tokens.push_back(Token{T_GT, ">", line}); break;
                case ':': tokens.push_back(Token{T_COLON, ":", line});break;
                case '<': tokens.push_back(Token{T_LT, ">", line}); break;
            default:
                cout << "Unexpected character: " << current << " at line " << line << endl;
                exit(1);
            }
            pos++;
        }
        tokens.push_back(Token{T_EOF, "", line});
        return tokens;
    }

    string  consumeNumber() {
            size_t start = pos;
            bool hasDecimalPoint = false;

            while (pos < src.size() && (isdigit(src[pos]) || (src[pos] == '.' && !hasDecimalPoint))) {
                if (src[pos] == '.') {
                    hasDecimalPoint = true;
                }
                pos++;
            }

            return src.substr(start, pos - start);
    }

    string consumeWord()
    {
        size_t start = pos;
        while (pos < src.size() && isalnum(src[pos]))
            pos++;
        return src.substr(start, pos - start);
    }

    string consumeString()
    {
        pos++; 
        size_t start = pos;
        while (pos < src.size() && src[pos] != '"')
            pos++;
        string str = src.substr(start, pos - start);
        pos++; 
        return str;
    }
};
class SymbolTable
{
    public:
        void addSymbol(const string &name, const string &type)
        {
            if (symbolTable.find(name) != symbolTable.end())
            {
                cout<<"Semantic error: Variable '" + name + "' is already declared."<<endl;
            }
            symbolTable[name] = type;
        }

        string getSymbolType(const string &name)
        {
            if (symbolTable.find(name) == symbolTable.end())
            {
                cout<<"Semantic error: Variable '" + name + "' is not declared."<<endl;
            }
            return symbolTable[name];
        }

        bool isDeclared(const string &name)
        {
            return symbolTable.find(name) != symbolTable.end();
        }
        void printSymbolTable() const {
        cout << "Symbol Table:" << endl;
        for (const auto &symbol : symbolTable) {
            cout << "Name: " << symbol.first << ", Type: " << symbol.second << endl;
        }
    }

    private:
        map<string, string> symbolTable;
};


class Parser {
 
    private:
        vector<Token> tokens;
        size_t pos;
        SymbolTable symTable;
    public:
        Parser(const vector<Token> &tokens) {
            this->tokens = tokens;  
            this->pos = 0;          
        }

        void parseProgram() {
            while (tokens[pos].type != T_EOF) {
                parseStatement();
                cout << "Symbol Table:\n" << endl;
                symTable.printSymbolTable();
            }
            cout << "Parsing completed successfully! No Syntax Error" << endl;
        }


    void parseStatement() {
        if (tokens[pos].type == T_INT || tokens[pos].type == T_FLOAT || tokens[pos].type == T_DOUBLE || tokens[pos].type == T_BOOL || tokens[pos].type == T_STRING) {
            parseDeclaration(tokens[pos].type);                
        } else if (tokens[pos].type == T_ID) {
            parseAssignment();
        } else if (tokens[pos].type == T_IF) {
            parseIfStatement();
        } else if (tokens[pos].type == T_RETURN) {
            parseReturnStatement();
        } else if (tokens[pos].type == T_LBRACE) {  
            parseBlock();
        }
        else if (tokens[pos].type == T_WHILE) {
            parseWhileStatement();
        }

        else {
            cout << "Syntax error: unexpected token " << tokens[pos].value << "' at line " << tokens[pos].line;
            exit(1);
        }
    }
    void parseBlock()
    {
        expect(T_LBRACE);
        while (tokens[pos].type != T_RBRACE && tokens[pos].type != T_EOF)
        {
            parseStatement(); 
        }
        expect(T_RBRACE);
    }
    void parseWhileStatement() {
        expect(T_WHILE);  
        expect(T_LPAREN); 
        parseExpression();
        expect(T_RPAREN);
        parseStatement();
    }

    void parseDeclaration(TokenType type) {

        expect(type);                      
        string name = expectGetValue(T_ID);
        string data_type = "";
        if(type == T_INT) {data_type = "int";}
        else if(type == T_FLOAT) {data_type = "float";}
        else if(type == T_BOOL) {data_type = "bool";}
        else if(type == T_STRING) {data_type = "string";}
        else if(type == T_DOUBLE) {data_type = "double";}
            
        symTable.addSymbol(name, data_type);    
        expect(T_SEMICOLON);
    }
    string expectGetValue(TokenType type)
    {
        string value = tokens[pos].value;
        expect(type);
        return value;
    }

    void parseAssignment()
    {
        string name = expectGetValue(T_ID);
        symTable.getSymbolType(name);
        expect(T_ID);
        expect(T_ASSIGN);
        parseExpression();
        expect(T_SEMICOLON);
    }
    

    void parseIfStatement() {
        expect(T_IF);
        expect(T_LPAREN);
        parseExpression();
        expect(T_RPAREN);
        parseStatement();  
        if (tokens[pos].type == T_ELSE) {
            expect(T_ELSE);
            parseStatement();  
        }
    }

    void parseReturnStatement() {
        expect(T_RETURN);
        parseExpression();
        expect(T_SEMICOLON);
    }

    void parseExpression() {
        parseTerm();
        while (tokens[pos].type == T_PLUS || tokens[pos].type == T_MINUS) {
            pos++;
            parseTerm();
        }
        if (tokens[pos].type == T_GT || tokens[pos].type == T_LT) {
            pos++;
            parseExpression();  
        }
    }

    void parseTerm() {
        parseFactor();
        while (tokens[pos].type == T_MUL || tokens[pos].type == T_DIV) {
            pos++;
            parseFactor();
        }
    }

    void parseFactor() {
        if (tokens[pos].type == T_NUM || tokens[pos].type == T_ID || tokens[pos].type == T_STRING ) {
            pos++;
        } else if (tokens[pos].type == T_LPAREN) {
            expect(T_LPAREN);
            parseExpression();
            expect(T_RPAREN);
        }
        else if(tokens[pos].type == T_TRUE || tokens[pos].type == T_FALSE)
        {   
            pos++;
            
        }
        else {
            cout << "Syntax error: unexpected token " << tokens[pos].value << endl;
            exit(1);
        }
    }

    void expect(TokenType type) {
        if (tokens[pos].type == type) {
            pos++;
        } else {
            cout << "Syntax error: expected token of type " << type << " but found '" 
                << tokens[pos].value << "' at line " << tokens[pos].line << endl;
            exit(1);
        }
    }
};


int main(int argc, char* argv[]) {
    if (argc < 2) {
        cerr << "Usage: " << argv[0] << " <filename>" << endl;
        return 1;
    }
    ifstream file(argv[1]);
    if (!file) {
        cerr << "Error: Could not open file " << argv[1] << endl;
        return 1;
    }

    string input, line;
    while (getline(file, line)) {
        input += line + "\n";
    }

    file.close();

    Lexer lexer(input);
    vector<Token> tokens = lexer.tokenize();
    
    Parser parser(tokens);
    parser.parseProgram();

    return 0;
}