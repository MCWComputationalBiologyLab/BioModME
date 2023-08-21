import numpy as np

def remove_random_spikes(data, threshold=3):
    """
    Removes random spikes from the given data array.

    Args:
        data (array): The data array.
        threshold (float): The threshold for identifying spikes.

    Returns:
        The cleaned data array.
    """
    # Identify spikes based on the difference between each data point and its neighboring points
    diff = np.abs(np.diff(data))

    # Calculate threshold by removing outliers 
    median = np.median(diff)
    cutoff = median * threshold
  

    spikes = np.concatenate(([False], diff > cutoff, [False]))

    # Replace spikes with the average of the neighboring data points
    cleaned_data = data.copy()
    for i in range(len(data)):
        if spikes[i]:
            cleaned_data[i] = np.mean(data[i-1:i+2])

    return cleaned_data