#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <memory>
#include <algorithm>

using namespace std;

enum class Type
{
  NUMBER, STRING
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
  
  ~Val()
  {
  }
};

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
private:
  map<string, Val> myBindings;
  vector<Env> children;
};

struct Stmt
{
  string stmt_str;
  Stmt() : stmt_str("") {}
  Stmt(string s) : stmt_str(s) {}
  virtual void eval(Env& env) = 0;
  void debug_print() const
  {
    cout << "Eval: " << str() << endl;
  }
  string str() const 
  {
    return stmt_str;
  }
};

struct LetStmt : public Stmt
{
  LetStmt(string s): Stmt(s) {}
  
  virtual void eval(Env& env)
  {
    debug_print();
  }
};

struct AssignmentStmt : public Stmt
{
  AssignmentStmt(string s) : Stmt(s) {}
  
  virtual void eval(Env& env)
  {
    debug_print();
  }
};

struct GotoStmt : public Stmt
{
  GotoStmt(string s) : Stmt(s) {}
  
  virtual void eval(Env& env)
  {
    debug_print();
  }
};

class Program : public Stmt
{
public:
  Program() : minline(0), maxline(0), currentline(0)
  {
  }
  
  virtual void eval(Env& env)
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
  p.at(10, make_unique<LetStmt>("LET X = 10"));
  p.at(11, make_unique<GotoStmt>("GOTO 20"));
  p.at(20, make_unique<AssignmentStmt>("X = X + Y^2"));
  p.at(12, make_unique<LetStmt>("LET Y = 20"));
  
  p.listing();
  p.eval(env);
  return 0;
}

