#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <memory>
#include <algorithm>

using namespace std;

enum class Type
{
    NUMBER, STRING, NIL
};

const string GOTOLINE = "GOTOLINE";

struct Val
{
    double num;
    string str;
    Type type;

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
        }
    }

    static Val nil();

    static Val make_number(double);

    static Val make_string(string);

    ~Val()
    {
    }
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
            cout << ">> " << p.first << " " << p.second << endl;
        }
    }
private:
    map<string, Val> myBindings;
    vector<Env> children;
};

struct Stmt
{
    string stmt_str;

    Stmt() : stmt_str("") {}

    Stmt(string s) : stmt_str(s) {}

    virtual Val eval(Env& env) = 0;

    void debug_print() const
    {
        cout << "Eval: " << str() << endl;
    }

    string str() const
    {
        return stmt_str;
    }

    virtual ~Stmt()
    {}
};

struct LetStmt : public Stmt
{
    LetStmt(string s, string lhs, Stmt *rhs): Stmt(s), var(lhs), val_stmt(rhs) {}

    virtual Val eval(Env& env)
    {
        debug_print();
        Val value = val_stmt->eval(env);
        env.set(var, value);
        return Val::nil();
    }

    string var;
    Stmt *val_stmt;
};

struct AssignmentStmt : public Stmt
{
    AssignmentStmt(string s) : Stmt(s) {}

    virtual Val eval(Env& env)
    {
        debug_print();
        return Val::nil();
    }
};

struct GotoStmt : public Stmt
{
    GotoStmt(string s) : Stmt(s) {}

    virtual Val eval(Env& env)
    {
        debug_print();
        return Val::nil();
    }
};

struct NumStmt : public Stmt
{
    NumStmt(string s) : Stmt(s) {}

    virtual Val eval(Env& env)
    {
        debug_print();
        return Val::make_number(stod(stmt_str));
    }
};

struct StrStmt : public Stmt
{
    StrStmt(string s) : Stmt(s) {}

    virtual Val eval(Env& env)
    {
        debug_print();
        return Val::make_string(stmt_str);
    }
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
                cout << line << " " << f->second->str() << endl;
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
    p.at(10, make_unique<LetStmt>("LET X = 10", "X", new NumStmt("10")));
    p.at(12, make_unique<LetStmt>("LET Y = \"HELLO\"", "Y", new StrStmt("HELLO")));

    //p.listing();
    p.eval(env);
    env.dump();
    return 0;
}
