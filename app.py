import streamlit as st
import seaborn as sns
import matplotlib.pyplot as plt

def main():
    st.title("Hello Streamlit!")

    # Load dataset
    df = sns.load_dataset("geyser")

    # Sidebar configuration
    bins = st.sidebar.slider("Number of bins:", min_value=1, max_value=50, value=30)

    # Plotting
    fig, ax = plt.subplots()
    ax.hist(df['duration'], bins=bins, color='blue', edgecolor='black', alpha=0.7)
    ax.set_title("Histogram of Eruption Duration")
    ax.set_xlabel("Duration (minutes)")
    ax.set_ylabel("Frequency")
    st.pyplot(fig)

if __name__ == "__main__":
    main()
