#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <memory>
#include <algorithm>

#define DEBUG 0

using namespace std;

enum class Type
{
    NUMBER, STRING, NIL
};

const string GOTOLINE = "GOTOLINE";

struct Val
{
    Val()
    {
        num = 0;
        str = "";
        type = Type::NUMBER;
    }

    Val& operator=(const Val& other)
    {
        if (other.type == Type::STRING) {
            str = other.str;
        }
        else if (other.type == Type::NUMBER) {
            num = other.num;
        }

        type = other.type;

        return *this;
    }

    bool is_nil() const
    {
        return type == Type::NIL;
    }

    string as_string() const
    {
        switch (type) {
            case Type::STRING:
                return "\"" + str + "\"";
            case Type::NUMBER:
                return to_string(num);
            case Type::NIL:
                return "<nil>";
            default:
                return "";
        }
    }

    static Val nil();

    static Val make_number(double);

    static Val make_string(string);

    ~Val()
    {
    }

    double num;
    string str;
    Type type;
};

Val Val::nil()
{
    Val v;
    v.type = Type::NIL;

    return v;
}

Val Val::make_number(double n)
{
    Val v;
    v.type = Type::NUMBER;
    v.num = n;

    return v;
}

Val Val::make_string(string s)
{
    Val v;
    v.type = Type::STRING;
    v.str = s;

    return v;
}

ostream& operator<<(ostream& out, const Val& val)
{
    out << val.as_string();
    return out;
}

class Env
{
public:
    Env()
    {
    }

    void set(string var, Val val)
    {
        myBindings[var] = val;
    }

    bool lookup(string var, Val *val)
    {
        auto f = myBindings.find(var);

        if (f != myBindings.end()) {
            *val = f->second;
            return true;
        }

        return false;
    }

    void remove(string var)
    {
        myBindings.erase(var);
    }

    void dump() const
    {
        for (const auto& p : myBindings) {
            cout << "Env> " << p.first << " " << p.second << endl;
        }
    }
    
    void set_goto(int line)
    {
        this->set(GOTOLINE, Val::make_number(line));
    }
private:
    map<string, Val> myBindings;
    vector<Env> children;
};

class Stmt
{
public:
    Stmt() : stmt_str("") {}

    Stmt(string s) : stmt_str(s) {}

    virtual Val eval(Env& env) = 0;

    void debug_print() const
    {
        if (DEBUG) {
            cout << "Eval: " << as_string() << endl;
        }
    }

    string as_string() const
    {
        return stmt_str;
    }

    virtual ~Stmt()
    {
        //cout << "~DEL " << stmt_str << endl;
    }
protected:
    string stmt_str;
};

class LetStmt : public Stmt
{
public:
    LetStmt(string s, string lhs, unique_ptr<Stmt> rhs): 
        Stmt(s), var(lhs), val_stmt(move(rhs)) {}

    virtual Val eval(Env& env)
    {
        debug_print();
        Val value = val_stmt->eval(env);
        env.set(var, value);
        return value;
    }
private:
    string var;
    unique_ptr<Stmt> val_stmt;
};

class AssignmentStmt : public Stmt
{
public:
    AssignmentStmt(string s, string lhs, unique_ptr<Stmt> rhs): 
        Stmt(s), var(lhs), val_stmt(move(rhs)) {}

    virtual Val eval(Env& env)
    {
        debug_print();
        Val value = val_stmt->eval(env);
        Val temp;
        bool f = env.lookup(var, &temp);

        if (!f) {
            cerr << "warn: assignment without LET has no effect!" << endl;
            return Val::nil();
        }

        env.set(var, value);
        return value;
    }
private:
    string var;
    unique_ptr<Stmt> val_stmt;
};

class GotoStmt : public Stmt
{
public:
    GotoStmt(string s, unique_ptr<Stmt> ln) : 
        Stmt(s), line(move(ln)) {}

    virtual Val eval(Env& env)
    {
        debug_print();
        Val lineno = line->eval(env);
        if (lineno.type == Type::NUMBER) {
            env.set_goto((int)lineno.num);
        }
        return Val::nil();
    }
private:    
    unique_ptr<Stmt> line;
};

class NumStmt : public Stmt
{
public:
    NumStmt(string s) : Stmt(s) {}

    virtual Val eval(Env& env)
    {
        debug_print();
        return Val::make_number(stod(stmt_str));
    }
};

class StrStmt : public Stmt
{
public:
    StrStmt(string s) : Stmt(s) {}

    virtual Val eval(Env& env)
    {
        debug_print();
        return Val::make_string(stmt_str);
    }
};

class VarStmt : public Stmt
{
public:
    VarStmt(string s) : Stmt(s) {}
    
    virtual Val eval(Env& env) {
        debug_print();
        Val v;
        env.lookup(stmt_str, &v);
        return v;
    }
};

class FuncallStmt : public Stmt
{
public:
    FuncallStmt(string s, string fn_name) : Stmt(s), name(fn_name)
    {
    }
    
    void add_arg(unique_ptr<Stmt> arg)
    {
        args.push_back(move(arg));
    }
        
    virtual Val eval(Env& env)
    {
        vector<Val> eargs;
        eargs.reserve(args.size());
        
        for (auto& x : args) {
            eargs.push_back(x->eval(env));
        }
        
        // Just do a PRINT
        // PRINT, INPUT etc. will be builtin functions
        for (auto x : eargs) {
            cout << x << " ";
        }
        
        cout << endl;
        
        return Val::nil();
    }
private:
    vector<unique_ptr<Stmt>> args;
    string name;
};

class Program : public Stmt
{
public:
    Program() : minline(0), maxline(0), currentline(0)
    {
    }

    virtual Val eval(Env& env)
    {
        while (currentline <= maxline) {
            auto stmt = statements.find(currentline);

            if (stmt != statements.end()) {
                stmt->second->eval(env);
            }

            Val gotoline;
            if (env.lookup(GOTOLINE, &gotoline)) {
                // reset delete gotoline, or else infinite loop
                env.remove(GOTOLINE);
                if (gotoline.type == Type::NUMBER) {
                    this->reset_line(gotoline.num);
                }
            }
            else {
                currentline++;
            }
        }

        return Val::nil();
    }

    void at(int line, unique_ptr<Stmt> ustmt_ptr)
    {
        if ((line < 0) || (line > 9999)) line = 0;
        minline = min(line, minline);
        maxline = max(line, maxline);
        statements[line] = move(ustmt_ptr);
    }

    void listing() const
    {
        for (int line = minline; line <= maxline; ++line) {
            auto f = statements.find(line);

            if (f != statements.end()) {
                cout << line << " " << f->second->as_string() << endl;
            }
        }
    }

    void reset_line(int line = 0)
    {
        if ((line >= minline) && (line <= maxline)) {
            auto f = statements.find(line);
            if (f == statements.end()) {
                cerr << "warn: no program label " << line << " found, starting from next available..."
                     << endl;
            }
            currentline = line;
        }
    }

private:
    int minline;
    int maxline;
    int currentline;
    map<int, unique_ptr<Stmt>> statements;
};

int main()
{
    Program p;
    Env env;
    
    p.at(10, make_unique<LetStmt>("LET X = 10.67", "X", make_unique<NumStmt>("10.67")));
    p.at(12, make_unique<LetStmt>("LET Y = \"HELLO\"", "Y", make_unique<StrStmt>("HELLO")));
    p.at(14, make_unique<GotoStmt>("GOTO 20", make_unique<NumStmt>("20")));
    p.at(18, make_unique<AssignmentStmt>("X = \"GELLO\"", "Y", make_unique<StrStmt>("GELLO")));
    p.at(20, make_unique<AssignmentStmt>("Y = \"PELLO\"", "Y", make_unique<StrStmt>("PELLO")));
    unique_ptr<FuncallStmt> fstmt = make_unique<FuncallStmt>("PRINT \"HI\", 3.14, X", "PRINT");
    fstmt->add_arg(make_unique<StrStmt>("HI"));
    fstmt->add_arg(make_unique<NumStmt>("3.14"));
    fstmt->add_arg(make_unique<VarStmt>("X"));
    p.at(22, move(fstmt));
    p.listing();
    cout << "----" << endl;
    p.eval(env);
    cout << "----" << endl;
    env.dump();
    return 0;
}
