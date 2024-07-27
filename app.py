import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Initialize session state
if 'legs_data' not in st.session_state:
    st.session_state['legs_data'] = []

def calculate():
    # Example of processing input data
    data = st.session_state['legs_data']
    # Your logic here to calculate profit/loss and identify strategy

    # Dummy data for plotting
    stock_prices = np.linspace(100, 200, 100)
    profits = stock_prices * 0.5  # Dummy calculation

    # Plot
    fig, ax = plt.subplots()
    ax.plot(stock_prices, profits, label='Profit/Loss')
    ax.set_xlabel('Stock Price at Expiry')
    ax.set_ylabel('Profit/Loss')
    ax.set_title('Profit/Loss Plot')
    ax.legend()
    st.pyplot(fig)

    # Update summary table
    st.table(data)

def add_leg():
    leg_data = {
        'Direction': st.selectbox('Direction', ['Buy', 'Sell'], key='direction'),
        'Option Type': st.selectbox('Option Type', ['Call', 'Put'], key='option_type'),
        'Strike Price': st.number_input('Strike Price', key='strike_price'),
        'Premium': st.number_input('Premium', key='premium'),
        'Quantity': st.number_input('Quantity', min_value=1, key='quantity'),
        'Contract Size': st.number_input('Contract Size', min_value=1, key='contract_size')
    }
    st.session_state['legs_data'].append(leg_data)

# Streamlit layout
st.title("Dynamic Option Strategy Calculator")

num_legs = st.number_input("Number of Legs", min_value=1, max_value=10, step=1)
for _ in range(num_legs):
    add_leg()

if st.button("Calculate"):
    calculate()
