struct my_state_struct {
  int val;
  int state;
};
typedef struct my_state_struct my_state;


int testfn(int a, int s, my_state * result);

int f1(int a, int s, my_state * result);
int f2(int a, int s, my_state * result);
int f3(int a, int s, my_state * result);
