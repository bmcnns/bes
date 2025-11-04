# Acrobot (10 runs)
#for i in {1..1}; do
#  sbcl --noinform --disable-debugger \
#       --load ~/Repos/bes/experiments/acrobot.lisp \
#       --quit
#done

# MountainCar (10 runs)
#for i in {1..10}; do
#  sbcl --noinform --disable-debugger \
#       --load ~/Repos/bes/experiments/mountain-car.lisp \
#       --quit
#done

# CartPole (10 runs)
#for i in {1..1}; do
#  sbcl --noinform --disable-debugger \
#       --load ~/Repos/bes/experiments/cartpole.lisp \
#       --quit
#done

# LunarLander (10 runs)
for i in {1..10}; do
  sbcl --noinform --disable-debugger \
       --load ~/Repos/bes/experiments/lunar-lander.lisp \
       --quit
done
