# Build a neural network model that classifies whether it is a mammal or a bird based on whether it has fur and wings
import tensorflow as tf
import numpy as np

# [fur, wing]
x_data = np.array(
    [[0, 0], [1, 0], [1, 1], [0, 0], [0, 0], [0, 1]])

# [etc, mammal, bird]
y_data = np.array([
    [1, 0, 0],  # etc
    [0, 1, 0],  # mammal
    [0, 0, 1],  # bird
    [1, 0, 0],
    [1, 0, 0],
    [0, 0, 1]
])

########################
# Neural Network Model #
########################
X = tf.placeholder(tf.float32)
Y = tf.placeholder(tf.float32)

# [features, the number of neurons in hidden layer] -> [2, 10]
W1 = tf.Variable(tf.random_uniform([2, 10], -1., 1.))
# [the number of neurons in 1st hidden layer, the number of categories] -> [10, 3]
W2 = tf.Variable(tf.random_uniform([10, 3], -1., 1.))

# Set bias value to the number of outputs for each layer
# b1 -> the number of neurons in hidden layer, b2 -> the number of categories
b1 = tf.Variable(tf.zeros([10]))
b2 = tf.Variable(tf.zeros([3]))

# Apply weight W1 and bias b1 to the hidden layer of the neural network
L1 = tf.add(tf.matmul(X, W1), b1)
L1 = tf.nn.relu(L1)

# Compute the final output
# A second weight W2 and bias b2 are applied to the hidden layer to produce three outputs
model = tf.add(tf.matmul(L1, W2), b2)

# Using the cross entropy function provided by default in TensorFlow
# You can simply apply the cost function for optimization without using complex formulas as follows
cost = tf.reduce_mean(
    tf.nn.softmax_cross_entropy_with_logits_v2(labels=Y, logits=model))

optimizer = tf.train.AdamOptimizer(learning_rate=0.01)
train_op = optimizer.minimize(cost)


########################
# Neural Network Train #
########################
init = tf.global_variables_initializer()
sess = tf.Session()
sess.run(init)

for step in range(100):
    sess.run(train_op, feed_dict={X: x_data, Y: y_data})

    if (step + 1) % 10 == 0:
        print(step + 1, sess.run(cost, feed_dict={X: x_data, Y: y_data}))


##############################
# Results                    #
# 0: etc, 1: mammal, 2: bird #
##############################
prediction = tf.argmax(model, 1)
target = tf.argmax(Y, 1)
print('Prediction:', sess.run(prediction, feed_dict={X: x_data}))
print('Y:', sess.run(target, feed_dict={Y: y_data}))

is_correct = tf.equal(prediction, target)
accuracy = tf.reduce_mean(tf.cast(is_correct, tf.float32))
print('Accuracy: %.2f' % sess.run(accuracy * 100, feed_dict={X: x_data, Y: y_data}))
