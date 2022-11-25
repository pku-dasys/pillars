## Jupyter Notebook for pillars

### 1. Check the existence of Scala for Jupyter

```
jupyter kernelspec list
```
You may skip the next step when a scala kernel exists.

### 2. Install [Jupyter Backend for Scala](https://github.com/freechipsproject/chisel-bootcamp/blob/master/Install.md#jupyter-backend-for-scala)

```
curl -L -o coursier https://git.io/coursier-cli && chmod +x coursier
SCALA_VERSION=2.12.10 ALMOND_VERSION=0.9.1
./coursier bootstrap -r jitpack \
    -i user -I user:sh.almond:scala-kernel-api_$SCALA_VERSION:$ALMOND_VERSION \
    sh.almond:scala-kernel_$SCALA_VERSION:$ALMOND_VERSION \
    --sources --default=true \
    -o almond
./almond --install
```

### 3. Open Start ``hello.ipynb`` in Jupyter Notebook

```
jupyter notebook
```

